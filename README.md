research code (read: experimental, unreadable and full of quick hacks :)) intended to help to finally prove [bb(5)](https://en.wikipedia.org/wiki/Busy_beaver#Known_values_for_%CE%A3_and_S), see **[bbchallenge.org](https://bbchallenge.org)**.


to run this code:

- you need recent nightly rust compiler, you can get one with [rustup](https://rustup.rs/)
- to speed up compilation (under linux), project uses [mold](https://github.com/rui314/mold) linker - install it or remove `.cargo/config.toml`


there are 3 main binaries; you can find example how to use them somewhere around main function:

- `tui` - detailed view of proving one machine
- `validate` - compare the generated proofs with other bbchallenge's "deciders"
- `prove` - prove machines en masse 
