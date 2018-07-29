# opengl-sandbox

install:
```
brew install sdl2 pkg-config
stack init
```

development:
```
stack build --file-watch --fast --exec "stack exec opengl-sandbox-exe"
```
