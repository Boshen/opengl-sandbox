module Program where

import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import qualified Graphics.Rendering.OpenGL as GL
import           SDL                       (($=))

import           LoadShaders

terrain = "terrain"

programs :: [(String, String, String)]
programs = [(terrain, "./shaders/cube.vert", "./shaders/cube.frag")]

uniforms :: [(String, GL.AttribLocation)]
uniforms =
  [ ("model", GL.AttribLocation 0)
  , ("view", GL.AttribLocation 1)
  , ("projection", GL.AttribLocation 2)
  , ("objectColor", GL.AttribLocation 3)
  , ("lightColor", GL.AttribLocation 4)
  , ("lightPos", GL.AttribLocation 5)
  ]

buildPrograms :: IO (Map String GL.Program)
buildPrograms = Map.fromList <$> mapM buildProgram programs

buildProgram :: (String, String, String) -> IO (String, GL.Program)
buildProgram (name, vert, frag) = do
  program <-
    loadShaders
      [ ShaderInfo GL.VertexShader (FileSource vert)
      , ShaderInfo GL.FragmentShader (FileSource frag)
      ]
  mapM_ (\(name, loc) -> GL.attribLocation program name $= loc) uniforms
  return (name, program)

setUniform program name d = do
  location <- GL.uniformLocation program name
  GL.uniform location $= d

getTerrainProgram :: Map String GL.Program -> GL.Program
getTerrainProgram programs = programs Map.! terrain
