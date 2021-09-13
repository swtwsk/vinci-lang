# Vinci : a toy ML shading language

**Vinci** is a toy ML functional shading language. This repository contains its dedicated compiler targeting [SPIR-V](https://www.khronos.org/registry/SPIR-V/specs/unified1/SPIRV.html), an intermediate representation of [Vulkan](https://www.vulkan.org/) shaders.

The language and the compiler were developed as a part of a master's thesis at the [Faculty of Mathematics, Informatics and Mechanics](https://www.mimuw.edu.pl/) of the University of Warsaw (MIM UW). The [extended abstract](docs/icfp_abstract.pdf) of the thesis, prepared for ICFP 2021 SRC, can be found inside the `docs` directory.

## Installation
The project uses [Stack build tool](https://www.haskellstack.org) to compile and manage dependencies.

Installing the compiler:
```bash
stack build
stack install
```

Stack will install the compiler in `~/.local/bin/vinci-lang-exe`, it can be then run by calling `vinci-lang-exe`.

## Usage
Assuming the compiler is installed in `~/.local/bin/`, running
```bash
vinci-lang-exe examples/test_frag.vc > examples/test_frag.spvasm
```
will produce a SPIR-V module in a human-readable representation. It should be further compiled using `spirv-as`, part of the [SPIR-V Tools](https://github.com/KhronosGroup/SPIRV-Tools) that gets installed along [Vulkan SDK](https://vulkan.lunarg.com/sdk/home):
```bash
spirv-as --target-env vulkan1.1 examples/test_frag.spvasm -o examples/test_frag.spv
```
`spirv-as` creates a SPIR-V binary module from the human-readable representation, that can be executed by a program using Vulkan API, like [this one](https://github.com/swtwsk/vulkan-playground).

![Example of Vinci compiler usage](docs/example.gif)

## Code example
**Vinci** is similar to other languages from ML family. In contrast to those languages, Vinci does not support algebraic data types or pattern matching due to performance concerns and limitations of GPU programming. On the other hand, apart from expressions typical for toy ML languages — e.g., arithmetic expressions, `let` bindings, or tuples — it also introduces C-like structures (similar to records from functional programming) and tuple destructuring syntax.

Code examples of fragment and vertex shaders are to be found in the `examples` directory. The fragment shader example displays a moving checkerboard (shown in the GIF above), the vertex shader presented below flips the 2D object every moment:

```ocaml
# structure definitions omitted

let pi = 3.1415926538;;

let vert (u : Uniforms) (ins : VertIns) =
  let (r, g, b) = ins.inColor in
  let (x, y) = ins.inPosition in
  let t = 1.0 + sin (u.ubo.u_time - pi / 2.0) in
  let f1 x = x - t and f2 x = x + t in
  let y2 = if y > 0.0 then f1 y else f2 y in
  VertOuts {
    gl_Position = (x, y2, 0.0, 1.0),
    fragColor = (r, g, b, 1.0),
    fragTexCoord = ins.inTexCoord
  }
;;
```

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[MIT](https://choosealicense.com/licenses/mit/)
