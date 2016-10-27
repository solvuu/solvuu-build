# Solvuu's build system.

`solvuu-build` makes it easy for you to build OCaml projects. You
express your build in a single OCaml file. Simple projects require
only a few lines of code and then a call to `make` will do lots of
useful things: compile the OCaml code of course but also generate
.merlin, .ocamlinit, Makefile files and more. Since the build
configuration is in OCaml, you have the full power of OCaml to add
complex (or simple) build rules if needed.

## Quickstart
Install by doing `opam install solvuu-build`. In your repo, create a
Makefile with the single line

```make
include $(shell opam config var solvuu-build:lib)/solvuu.mk
```

Write some OCaml code in one or more files, and place them in a
sub-directory called `lib` (or another name of your choice).

Create a file called `myocamlbuild.ml` at your repo's root with the
following content:

```ocaml
open Solvuu_build.Std

let project_name = "my-project"
let version = "dev"

let mylib = Project.lib project_name
  ~dir:"lib"
  ~style:`Basic
  ~pkg:project_name

let () = Project.basic1 ~project_name ~version [mylib]
```

Type `make`. If anything goes wrong, you might launch utop and do
`#use myocamlbuild.ml` to debug.

In the call to `Project.lib` above, the unlabeled argument is the name
of the library, which dictates the basename of `.cma` and `.cmxa`
files. Then, we specified the directory in which your library's files
reside. By default all `.ml`, `.mli`, and `.c` files therein will be
compiled into your library. The `pkg` argument is the findlib package
name you want to assign to this library. Finally, the style is set to
`Basic`. Also supported is `Pack` and there is an open feature request
to support _module aliases as a namespace_. Compilation can be
customized via several optional arguments, such as `safe_string`.

Here's what you get:

- `.cmo`, `.cmi`, `.cma`, etc: Files output by `ocamlc`. By default,
  only byte code compilation is done to save time during
  development. Run `make native` to also get the corresponding
  `ocamlopt` output.

- `.ocamlinit`: An ocamlinit file for use during development. It
  automatically loads the necessary findlib dependencies (none in our
  simple example above) and your compiled `.cma`. Thus, doing `utop`
  from your repo root let's you immediately use your own code.

- `.merlin`: A merlin file that correctly includes all the source and
  build directories, and findlib dependencies. This is generated
  first, so even if you have a compilation error in your OCaml code,
  you can start benefiting from merlin right away in your editor.

- `_build/project.mk`: You might be wondering how all the above
  happens with just a call to `make`. A makefile is auto-generated and
  the single line of code you wrote in your Makefile ends up including
  this one. Note in particular the rule targeting `_build/%`. You can
  thus ask to build any artefact under _build with a simple `make
  _build/lib/foo.cmo` for example. The corresponding call to
  ocamlbuild is more verbose.

- `_build/META`: A findlib META file. It correctly handles
  dependencies and multi-library projects. Not built by default. Run
  `make _build/META` to generate it.

- `<project_name>.install`: A .install file as needed by OPAM. Not
  built by default. Run `make <project_name>.install` to generate it.

See the `demos` directory for other small examples.

## Users
Some real world examples where solvuu-build is being used:

- [biocaml](https://github.com/biocaml/biocaml/): Multiple libraries
  and multiple executable apps. One of the libraries contains a C
  file. Libraries include Async and Lwt variations, and they are
  compiled only if `async` or `lwt` are installed, respectively. It
  uses `m4` to automatically insert the git commit ID and project
  version into an `about.ml` file, which is compiled into the library.

- [cufp.org](https://github.com/CUFP/cufp.org): A library and app are
  compiled. The logic of doing the static site generation is encoded
  in the Makefile. These rules could all have been in
  `myocamlbuild.ml`, but this project demonstrates that sometimes Make
  is still better. Use both in combination.

- An [Eliom](#eliom) based website. No public project yet available.

- Also: [Coclobas](https://github.com/hammerlab/coclobas),
  [Future](https://github.com/solvuu/future),
  [Phat](https://github.com/solvuu/phat).


## Design Goals
Our motivation for this project was to stop thinking about building
code, so we can focus on writing code. Here's what we think is
required for that to happen:

- Entire build should be expressed in a single file.

- The language the build is expressed in should be OCaml.

- Nothing should happen by default. You should have to call at least
  one function to register any rules.

- API should be functional as much as possible.

- Should be possible to override every default. We do not fully
  succeed. Several decisions are currently hardcoded, and in some
  cases it isn't obvious how to make the decisions configurable. Or
  rather, making them all configurable would remove all benefits;
  the API would end up being "call the OCaml command line
  tools yourself".

- Complicated things should be possible. Examples: pre-process your file through
  cppo, convert an .md file to .html, download a file from the
  internet, run ocamldep and capture and parse its output during
  build, and much more. All of this should be possible. Actually, we
  hesitate to call these _complicated_ things. They're conceptually
  trivial, but for some reason very difficult to do within most build
  systems.

Technically `solvuu-build` is an ocamlbuild plugin. However, it avoids
ocamlbuild's builtin rules by calling
`Ocamlbuild_plugin.clear_rules()` as its first step. Ocamlbuild is
used only for its rule engine, not for any of the other features most
people associate it with. Here is a list of reasons we preferred to
avoid ocamlbuild's default rules:

- They are all registered by default. So even if your project has no
  parser in it, a rule for compiling `mly` files is registered. This
  invariably leads to esoteric error messages about how ocamlbuild
  knows of no rule to generate `foo.mly`, when actually your error
  is something entirely different.

- The rules are almost never what you need, which ocamlbuild
  recognizes. You certainly have your own choice for the `-w` warnings
  flag or whether or not to use `-safe-string`. Ocamlbuild's solution to
  this is to make all the rules it registers be rather
  complicated. They don't just make calls to the OCaml tools. Rather
  they all check whether an `_tags` file and various other side
  effecting functions have been called in your `myocamlbuild.ml`
  file. Based on a bunch of mutable state, each rule creates the
  specific underlying command that finally gets called. In other
  words, the default rules are actually parameterized (good) using a
  very complicated mechanism (bad). Solvuu-build uses a different
  technique to parameterize the rules that get registered:
  functions. Various functions are provided that when called will
  register one or more rules. We can make these functions take an
  arbitrarily rich amount of arguments, and it is clear how to call
  them because you already know OCaml. The only reason not to do it
  this way is if you want to avoid users having to call an OCaml
  function to configure their build, but we consider it a feature to
  use OCaml instead of other ad hoc syntax.

- Build configuration is split across multiple files. Any non-trivial
  project ends up having `myocamlbuild.ml`, `_tags`, and multiple `.cllib`
  and `.mllib` files.

- Much of the configuration can be done in `myocamlbuild.ml`, which
  ostensibly meets our goal of using OCaml as the configuration
  language. However, the API isn't really what we think of as
  OCaml. It is entirely imperative in nature.

- The implementation is essentially impossible to understand. The code
  uses mutable state everywhere and is largely undocumented.

- All paths are intrinsically relative to the `_build` directory, but
  some files are supposed to be generated outside `_build`
  (e.g. `.merlin`) and sometimes there is no benefit to copying or
  symlinking your source code into `_build`. This shouldn't be forced on
  you. It is also not the best UI; typing `ocamlbuild foo.cmo` actually
  builds `_build/foo.cmo`.


## Known Limitations

- Files for a single library cannot be spread across multiple
  directories. This might be unreasonable for very large projects, but
  most projects anyway adhere to this.

- Build paths are hardcoded. For example, if your library `foo`'s
  files are in a directory `lib`, then the `cma` file for that library
  will be built at `_build/foo.cma`. Some people might prefer to have
  it at `_build/lib/foo.cma`, but you cannot change this.

- True dynamic dependencies. We are limited by our use of ocamlbuild's
  rule engine, which kind of supports dynamic rules by passing a
  `build` function to the action of your rule. You can call `build`
  within your own action, and thus compute other targets at build
  time and dynamically call `build` to build them. This is nice and
  ends up working. However, you never truly generate a rule; you just
  run code that ends up doing stuff. The better solution would be for
  rules to form a monad, as in
  [Jenga](https://github.com/janestreet/jenga).

  As an example of why this matters, note that we are forced to
  mis-state the true dependencies of a packed `cmo`. We say the
  dependencies are all the `ml` files, but really they are all the
  `cmo` files produced by the `ml` files. The output of ocamlbuild's
  `-documentation` feature is thus misleading.


## Eliom

[Ocsigen](http://ocsigen.org/) provides a suite of libraries for web
programming. Most, such as [lwt](http://ocsigen.org/lwt/) and
[tyxml](http://ocsigen.org/tyxml/), can be used without any special
support. You simply add these to your list of findlib dependencies
when using the `Project` module. However,
[eliom](http://ocsigen.org/eliom/) works rather differently. Your
source files are compiled twice, once each for the server and client
side. We provide the `Eliom` module to support this.

Assume your source code is in a sub-directory "src" with files a.ml,
a.mli, b.eliom, b.eliomi, and c.ml. Further, assume module A is for
the server side only, but modules B and C are for both the client and
server side. Then, here's a complete `myocamlbuild.ml` file to compile
both native and byte code server libraries, and a JavaScript client
library.

```ocaml
open Solvuu_build.Std

let ml_files = function
  | `Server -> ["a.ml"; "b.eliom"; "c.ml"]
  | `Client -> [        "b.eliom"; "c.ml"]

let mli_files = function
  | `Server -> ["a.mli"; "b.eliomi"]
  | `Client -> [         "b.eliomi"]

let findlib_deps = function
  | `Server -> ["eliom.server"; "eliom.ppx.server"; "js_of_ocaml.ppx"; "core"]
  | `Client -> ["eliom.client"; "eliom.ppx.client"; "js_of_ocaml.ppx"]

let mylib =
  Eliom.lib "myweb"
    ~style:(`Pack "myweb")
    ~dir:"src"
    ~findlib_deps
    ~ml_files
    ~mli_files
    ~short_paths:()
    ~w:"A-4-33-41-42-44-45-48"

let () = Ocamlbuild_plugin.dispatch @@ function
| Ocamlbuild_plugin.After_rules -> (
    Ocamlbuild_plugin.clear_rules();
    Eliom.build_lib mylib;
  )
| _ -> ()
```

We recommend a Makefile like this:

```make
OCAMLBUILD=ocamlbuild -verbose 1 -use-ocamlfind -plugin-tag "package(solvuu-build)"

FORCE:
_build/%: FORCE
	$(OCAMLBUILD) $(patsubst _build/%, %, $@)

clean:
	rm -rf _build

```

Now type `make _build/_client/myweb.js` and `make _build/_server/myweb.cma`.

The `Eliom` module is less mature than `Project`. You don't get all
the extra nice things `Project` provides, such `.ocamlinit`,
`.merlin`, etc. Also, note that `Eliom` doesn't play well with
`Project`. At present, any given project should use only one or the
other, but this shouldn't be too hard to resolve.
