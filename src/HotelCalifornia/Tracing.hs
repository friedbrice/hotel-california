module HotelCalifornia.Tracing
    ( module HotelCalifornia.Tracing
    , defaultSpanArguments
    ) where

import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import Data.Time
import GHC.Stack
import OpenTelemetry.Context as Context hiding (lookup)
import OpenTelemetry.Context.ThreadLocal (attachContext)
import OpenTelemetry.Trace hiding
       ( SpanKind(..)
       , SpanStatus(..)
       , addAttribute
       , addAttributes
       , createSpan
       , inSpan
       , inSpan'
       , inSpan''
       )
import qualified OpenTelemetry.Trace as Trace
import qualified OpenTelemetry.Vendor.Honeycomb as Honeycomb
import UnliftIO

-- | Initialize the global tracing provider for the application and run an action
--   (that action is generally the entry point of the application), cleaning
--   up the provider afterwards.
--
--   This also sets up an empty context (creating a new trace ID).
withGlobalTracing :: MonadUnliftIO m => m a -> m a
withGlobalTracing act = do
    void $ attachContext Context.empty
    bracket initializeTracing shutdownTracerProvider $ \_ -> do
        -- note: this is not in a span since we don't have a root span yet so it
        -- would not wind up in the trace in a helpful way anyway
        void $
          Honeycomb.getOrInitializeHoneycombTargetInContext initializationTimeout
            `catch` \(e :: SomeException) -> do
              -- we are too early in initialization to be able to use a normal logger,
              -- but this needs to get out somehow.
              --
              -- honeycomb links are not load-bearing, so we let them just not come
              -- up if the API fails.
              liftIO . BS8.hPutStrLn stderr $ "error setting up Honeycomb trace links: " <> (BS8.pack $ displayException e)
              pure Nothing

        act
  where
    initializationTimeout = secondsToNominalDiffTime 3

initializeTracing :: MonadUnliftIO m => m TracerProvider
initializeTracing = do
  (processors, tracerOptions') <- liftIO getTracerProviderInitializationOptions
  provider <- createTracerProvider processors tracerOptions'
  setGlobalTracerProvider provider
  pure provider


globalTracer :: MonadIO m => m Tracer
globalTracer = getGlobalTracerProvider >>= \tp -> pure $ makeTracer tp "hotel-california" tracerOptions

inSpanWith :: (MonadUnliftIO m, HasCallStack) => Text -> SpanArguments -> m a -> m a
inSpanWith t a m = globalTracer >>= \tr -> Trace.inSpan tr t a m

inSpan :: (MonadUnliftIO m, HasCallStack) => Text -> m a -> m a
inSpan t = inSpanWith t defaultSpanArguments