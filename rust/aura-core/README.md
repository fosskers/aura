# `aura-core`

This crate contains core types and logic for running an Arch Linux-based package
manager like [Aura](https://github.com/fosskers/aura). Unlike a main executable,
it knows nothing about:

- which logging framework is being used
- how errors are handled
- how localisation is handled

Overall this approach allows the core to remain somewhat lean. It is a goal
overall to burden this core with as few dependencies as possible.
