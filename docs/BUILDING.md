---
---
# Dependencies

``` bash
# On Debian-based distro
sudo apt install bnfc latexmk texlive-generic-recommended txt2tags
```

# Building
``` bash
make
```

# Testing/Running on examples
``` bash
make run # run all examples
make runBad # run bad examples
make runGood # run good examples
make runWarn # run examples with warnings only
# there are also targets: test testBad testGood testWarn

# interpreter binary executed with --debug prints source code
./interpreter good/List.stl --debug
make run ARGS=--debug

```
