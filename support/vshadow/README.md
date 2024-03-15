# shadow

A lispy system for defining OpenGL shader programs and associated buffer objects.

## Overview

Under the hood, Shadow is just a wrapper around the [Varjo](https://github.com/cbaggers/varjo)
library used for writing shader programs, and some fluff to allow referencing shader programs by
name, querying for basic information about them, modifying uniform variables throughout the
lifecycle of an OpenGL application, and managing certain OpenGL buffer object types (UBO, SSBO
currently).

The goal of Shadow is to be a simple solution to ease the task of writing and managing OpenGL shader
programs and associated buffers.

## Install

``` lisp
(ql:quickload :shadow)
```

## Usage

### Basic Usage

Using Shadow is not very straightforward, mostly due to the borrowing of the "Vari" language used to
write shader programs, which does not have much documentation. It does however try to stay familiar
and resembles Common Lisp.  Additionally, there are [several
videos](https://www.youtube.com/watch?v=82o5NeyZtvw&list=PL2VAYZE_4wRITJBv6saaKouj4sWSG1FcS) of
Vari's usage created by its author.

Shader programs are written using a series of `DEFUN` and `DEFSTRUCT` forms representing GPU
functions and structures respectively. `DEFUN` and `DEFSTRUCT` are special CL-like macros that live
in the `SHADOW.GLSL` package. As mentioned, their bodies follow the language rules of "Vari", which
is not documented here.  All shaders written should be in a dedicated package you create that
`:USE`s `:SHADOW.GLSL`. This is a special package that takes care of shadowing the CL symbols and
exporting all the required symbols for writing shaders. It is not recommended to mix CL code and
shader code in the same package, to prevent having to fully-qualify `CL:DEFUN` everywhere. Please
use a dedicated package for writing your shaders which `:USE`s `:SHADOW.GLSL`.

Each `DEFUN` defines a shader stage or auxillary function thereof. It takes in input arguments and
uniform variables, and sends its return values to the next stage of the shader pipeline as input
arguments. The vertex stage's input arguments correspond to your Vertex Array Object attributes.

A simple OpenGL shader program:

```lisp
(defun foo-vert ((position :vec3) (uv :vec2) &uniforms (mvp :mat4))
  (values (* mvp (vec4 position 1))
          (vec4 1 0 0 1)))

(defun foo-frag ((color :vec4))
  (values color))
```

This defines 2 GPU functions, `foo-vert` and `foo-frag` that will serve as a very simple program
once translated and compiled.

To use this program it first must be translated from the Lisp-like "Vari" language, into GLSL. This
is done with the `DEFINE-SHADER` macro:

```lisp
(define-shader example-program (:version 330 :primitive :points)
  (:vertex (foo-vert :vec3 :vec2))
  (:fragment (foo-frag :vec4)))
```

Above, we call `DEFINE-SHADER` with a name to call our program, `EXAMPLE-PROGRAM`, the default stage
version to use, `:version 330`, and the OpenGL drawing primitive the vertex stage should use,
`:primitive :points`, followed by a sequence of "stage-specs" of the form: `(stage-type
function-spec)`:

`stage-type` may be one of: `:vertex`, `:tessellation-control`, `:tessellation-evaluation`,
`:geometry`, `:fragment`, or `:compute`.

`func-spec` specifies which `DEFUN` function to use for this stage, and is a list consisting of the
function name followed by the types of all of its input arguments. The types are important because
the "Vari" shader language allows the same function name to exist with different signatures, so you
must be explicit in which function you want to translate to GLSL.

Issuing the call to `DEFINE-SHADER` produces a `PROGRAM` object, which includes some useful
information:

The `SHADOW:VIEW-SOURCE` function can be used to retrieve the translated Varo -> GLSL source for a
given program and stage type:

```lisp
(define-shader ...)

(view-source * :vertex)

#|
"#version 330

layout(location = 0)  in vec3 POSITION;
layout(location = 1)  in vec2 UV;

out _FROM_VERTEX_STAGE_
{
     out vec4 _VERTEX_STAGE_OUT_1;
} v_out;

uniform mat4 MVP;

void main()
{
    gl_Position = (MVP * vec4(POSITION,float(1)));
    v_out._VERTEX_STAGE_OUT_1 = vec4(float(1),float(0),float(0),float(1));
    return;
}"
T
|#

(view-source ** :fragment)

#|
"#version 330

in _FROM_VERTEX_STAGE_
{
     in vec4 _VERTEX_STAGE_OUT_1;
} v_in;

layout(location = 0)  out vec4 _FRAGMENT_STAGE_OUT_0;

void main()
{
    _FRAGMENT_STAGE_OUT_0 = v_in._VERTEX_STAGE_OUT_1;
    return;
}"
T
|#
```

As can be seen by the GLSL source, our vertex stage function is properly making use of the `VALUES`
form. It takes the first value for itself, setting `gl_Position`, and passes all subsequent values
as input arguments to the fragment stage, `(vec4 1 0 0 1)`, which takes that for itself as the final
fragment color of the pipeline.

So far, we have only translated the "Vari" shader language into the GLSL language understood by
OpenGL. We still have to compile the shader stages and link the final program object on the GPU.

At this point, a valid OpenGL context is needed to continue.

To compile a program's stages and link them into a program, you can use the `BUILD-SHADER-PROGRAM`
function:

```lisp
(build-shader-program 'example-program)
```

This will compile all of the stages previously translated to GLSL in our `EXAMPLE-PROGRAM` program,
and link it into a program object on the GPU. This returns a non-zero integer on success.

Alternatively, you can compile and link all GLSL translated programs in one shot, by using the
`BUILD-SHADER-DICTIONARY` function, which takes no arguments and returns a hash table of all program
objects keyed by name.

`LOAD-SHADERS` can be used to automatically build all shader programs in Shadow's registry as well.
With `LOAD-SHADERS`, it takes 1 argument, which is a function. This user-supplied function, which
can be a `LAMBDA` takes a single argument which when called will be the list of shader programs
affected when a function is recompiled with `C-c C-c`. You can use this to automatically update
shaders while your program is running to see visual changes instantly. However, this requires some
effort on your part, because SLIME (and the Sly fork) recompiles forms in a separate compilation
thread when you `C-c C-c`. This is problematic because you must recompile shader programs on the
same thread as your OpenGL context. To get around this, you can do something like the following:

In your project's initialization step, load all shaders passing in a custom function which pushes
things to a thread-safe queue:

```lisp
(defun initialize-my-shaders (my-queue)
  (let ((modify-hook (lambda (queue-enqueue my-queue x))))
    (shadow:load-shaders modify-hook)))
```

Then periodically, you can dequeue items off of this queue. An item on the queue is a list of
programs that need to be recompiled, and the `RECOMPILE-SHADERS` function expects such a list of
program names and does the recompiling:

```lisp
(defun recompile-my-shaders (my-queue)
  (loop :while (queue-peek my-queue)
        :for programs = (queue-dequeue my-queue)
        :do (shadow:recompile-shaders programs)))
```

Shadow itself does not do this, nor does it have a thread-safe queue implementation. You'll have to
use a library such as [queues](https://github.com/oconnore/queues) or
[lparallel](https://github.com/lmj/lparallel) for that. However, it's pretty easy as you can see
above, and doing this will be extremely valuable, as you'll be able to see your GPU modifications
instantly while your project is running without the downtime of having to restart after every minor
change.

After compiling shaders, the only thing left to do, is make use of the shader program to do your
rendering. This is done by issuing calls to the various `UNIFORM-*` functions within the body of the
`WITH-SHADER` macro:

```lisp
(with-shader 'example-program
  (uniform-mat4 program :mvp *matrix*))
```

Here, we specify that we want to use `EXAMPLE-PROGRAM` during rendering, modifying a single 4x4
matrix uniform value. Here `*matrix*` refers to an imaginary matrix that you should have created for
the object you wish to render.  There are quite a few `UNIFORM-*` functions, and the full list can
be viewed in the [package's exported symbols](src/package.lisp). Note that each uniform function
takes a program object, the name of a uniform variable as a keyword symbol, followed by the value to
modify it with.

### UBO/SSBO Support

Shadow also includes support for uniform buffer objects (UBO's) and shader storage buffer objects
(SSBO's).

A buffer-backed interface block in Shadow is implemented as a struct with `DEFSTRUCT`. Anytime a
particular shader function wishes to read or write to this buffer, it must be specified in that
function's signature using the `&uniforms` part of its lambda list. To do this, you must know the
name of the struct, whether you want to access a UBO or SSBO buffer, and the packing layout of that
buffer (std140 or std430). For example, this function binds the symbol `var` using the previously
defined struct, `foo-block`, which will be later filled as an SSBO using the layout rules of the
std430 specification:

```lisp
(defun foo (&uniforms (var foo-block :ssbo :std430)
  ...))
```

This special uniform syntax must be present for each function that needs to access a buffer.

#### Creating block aliases

On the CPU side, we can create aliases for blocks. This is useful, because the same block name can
refer to multiple blocks, even in the context of the same shader program. To create a block alias,
use `CREATE-BLOCK-ALIAS`:

```lisp
(create-block-alias <block-type> <block-id> <program-name> <block-alias>)
```

* `<block-type>`: The keyword symbol :buffer or :uniform, depending if this block is a block which
  should be used with an SSBO or UBO, respectively.

* `<block-id>`: The name of the block. This is always a keyword symbol, derived from the name given
to the struct.

* `<program-name>`: A symbol denoting the name of the program where this block can be found, as
  defined with `DEFINE-SHADER`.

* `<block-alias>`: An identifier to be used to reference this block. May be a symbol, keyword
  symbol, or a case-sensitive string.

#### Deleting block aliases

It may be useful to delete a block alias. You can do so using
`DELETE-BLOCK-ALIAS`:

```lisp
(delete-block-alias <block-alias> &key unbind-block)
```

* `<block-alias>`: An identifier to be used to reference this block. May be a symbol, keyword
  symbol, or a case-sensitive string.

* `<unbind-buffer>`: When non-NIL, also disassociates the block from a binding point.

#### Referencing Blocks

To find a block object in Shadow's state, you can use `FIND-BLOCK`:

```lisp
(find-block <block-alias>)
```

* `<block-alias>`: A symbol, keyword symbol, or case-sensitive string denoting an alias previously
  defined with `CREATE-BLOCK-ALIAS`.

#### Binding Blocks

A block must be bound to a "binding point" for use. A buffer is then bound to this same binding
point to associate them with each other. To bind a block to a binding point, use `BIND-BLOCK`:

```lisp
(bind-block <block-alias> <binding-point>)
```

* `<block-alias>`: A symbol, keyword symbol, or case-sensitive string denoting an alias previously
defined with `CREATE-BLOCK-ALIAS`.

* `<binding-point>`: An integer to bind the block to. This ranges from 1 to a driver-dependent
  maximum.

#### Unbinding Blocks

To disassociate a block from a binding point, use `UNBIND-BLOCK`:

```lisp
(unbind-block <block-alias>)
```

* `<block-alias>`: A symbol, keyword symbol, or case-sensitive string denoting an alias previously
  defined with `CREATE-BLOCK-ALIAS`.

#### Creating Buffers

To create a buffer, you first need to create a block alias as per the above instructions. You can
then create a buffer which uses the layout of a particular block, using `CREATE-BUFFER`:

```lisp
(create-buffer <buffer-name> <block-alias>)
```

* `<buffer-name>`: A symbol that can later be used as a reference to the created
  buffer.

* `<block-alias>`: A symbol, keyword symbol, or case-sensitive string denoting
  an alias previously defined with `CREATE-BLOCK-ALIAS`.

#### Binding Buffers

To bind a buffer to a binding point, use `BIND-BUFFER`:

```lisp
(bind-buffer <buffer-name> <binding-point>)
```

* `<buffer-name>`: The name of a buffer that was defined with `CREATE-BUFFER`.

* `<binding-point>`: An integer to bind the buffer to. This ranges from 1 to a driver-dependent
  maximum.

#### Unbinding Buffers

To disassociate a buffer from a binding point, use `UNBIND-BUFFER`:

```lisp
(unbind-buffer <buffer-name>)
```

* `<buffer-name>`: The name of a buffer that was defined with `CREATE-BUFFER`.

#### Deleting Buffers

```lisp
(delete-buffer <buffer-name>)
```

* `<buffer-name>`: The name of a buffer that was defined with `CREATE-BUFFER`.


## License

Copyright © 2018-2020 Michael Fiano <mail@mfiano.net>.

Licensed under the MIT License.
