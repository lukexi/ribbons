{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import SDL.Pal hiding (get ,glBindTexture, createTexture, updateTexture)
import Graphics.GL.Pal
import Data.Time
import Halive.Utils
import Halive.FileListener
import Control.Lens.Extra hiding (Context)

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad
import Data.Monoid
import qualified Data.Set as S

import System.FilePath
import Graphics.GL.Ext.ARB.ShadingLanguageInclude
import Foreign.C

import Foreign hiding (void)

import Control.Concurrent
import Control.Arrow


type Transform = M44 GLfloat

data ShapeUniforms = ShapeUniforms
    { uTransform  :: UniformLocation Transform
    , uResolution :: UniformLocation (V2 GLfloat)
    , uMouse      :: UniformLocation (V2 GLfloat)
    , uTime       :: UniformLocation GLfloat
    } deriving Data

data RibbonState = RibbonState
    { _rsVertices :: [V2 GLfloat]
    , _rsUVs      :: [V2 GLfloat]
    }
makeLenses ''RibbonState

initialState = RibbonState [] []

main :: IO ()
main = do

    win <- reacquire 0 $ createGLWindow "ribbons"

    swapInterval $= SynchronizedUpdates

    glEnable GL_DEPTH_TEST
    glBlendEquationSeparate GL_FUNC_ADD GL_FUNC_ADD
    glBlendFuncSeparate GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA GL_ONE GL_ZERO

    -- putStr "gl_ARB_shading_language_include support? "
    -- print gl_ARB_shading_language_include

    getWatchedResource <- makeWatchedResource "resources/shapes.frag" $ do
        getDir "resources" >>= \ss -> buildNamedStrings ss ("/resources/"++)

        shader <- createShaderProgramInclude
            "resources/shapes.vert" "resources/shapes.frag" ["/resources"]
        useProgram shader

        uniforms <- acquireUniforms shader

        (quadVAO,  quadVerticesBuffer, quadUVsBuffer) <- makeScreenSpaceQuad shader
        return (shader, quadVAO, quadVerticesBuffer, quadUVsBuffer, uniforms)

    glEnable GL_DEPTH_TEST

    void . flip runStateT initialState . whileWindow win $ \events -> do
        V2 winFbW winFbH <- fmap fromIntegral <$> glGetDrawableSize win
        glViewport 0 0 winFbW winFbH
        glClearColor 0.1 0 0.1 1
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

        now <- realToFrac . utctDayTime <$> liftIO getCurrentTime
        winSize <- getWindowSizeV2 win
        mouseLoc <- getMouseLocationV2

        projection <- getWindowProjection win 45 0.1 1000



        (shader,
            quadVAO,
            quadVerticesBuffer,
            quadUVsBuffer,
            ShapeUniforms{..}) <- liftIO getWatchedResource


        -- Draw one fullscreen quad
        forM_ events $ \case
            Event { eventPayload =
                MouseButtonEvent (MouseButtonEventData
                    {mouseButtonEventMotion = Pressed})} -> do
                let position = mouseLoc' * 2 - 1
                    mouseLoc' = (mouseLoc / winSize) & _y %~ (1 -)
                rsVertices %= (position:)
                rsUVs      %= (position:)
            _ -> return ()

        RibbonState{..} <- get
        bufferSubData quadVerticesBuffer _rsVertices
        bufferSubData quadUVsBuffer _rsUVs

        let model = mkTransformation (axisAngle (V3 0 1 0) 0) (V3 0 0 0)
        useProgram shader
        uniformV2 uResolution winSize
        uniformV2 uMouse (mouseLoc & _y %~ (`subtract` (winSize^._y)))
        uniformF uTime now
        uniformM44 uTransform model
        withVAO quadVAO $
            glDrawArrays GL_TRIANGLE_STRIP 0 (fromIntegral $ length _rsVertices)


        glSwapWindow win

makeScreenSpaceQuad shader = do

    quadVAO <- newVAO
    quadVerticesBuffer <- bufferData GL_DYNAMIC_DRAW (replicate 100 0)
    quadUVsBuffer      <- bufferData GL_DYNAMIC_DRAW (replicate 100 0)
    withVAO quadVAO $ do
        withArrayBuffer quadVerticesBuffer $
            assignFloatAttribute shader "aPosition" GL_FLOAT 2
        withArrayBuffer quadUVsBuffer $
            assignFloatAttribute shader "aUV" GL_FLOAT 2

    return (quadVAO, quadVerticesBuffer, quadUVsBuffer)
