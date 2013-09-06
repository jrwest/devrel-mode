# devrel-mode

`devrel-mode` is an emacs minor mode for working with devrel's while
developing Riak. It is quickly slapped together and probably poorly
written given its my first attempt at writing a mode in elisp.

## Assumptions

Some assumptions are made about how you layout your directories while
working on Riak. Specifically, `devrel-mode` assumes your directory
structure looks something like:

```
+ parent-dir
|
---- riak-dep-under-dev
---- riak
```

That is to say, you are working on some dependency of Riak and it is
cloned to `parent-dir`. In addition you have a clone of `basho/riak`
also inside `parent-dir` (so its a sibling directory of the
dependency). You should be able to work with multiple dependencies in
this structure (although its untested). If instead you prefer to
develop Riak by downloading the deps and editing their source files
directly (e.g. in `deps/<something>/src`) then `devrel-mode` does not
support you (yet -- it should be an easy change and sometimes I work
this way too. PR and/or beers accepted).

It is also assumed that you have built a devrel in the `riak`
directory once. `devrel-mode` is meant to take you from there
(although I'm sure it falls short in a few places).

`devrel-mode` also assumes you use
[edts](https://github.com/tjarvstrand/edts). Actually, you probably
don't need `edts-mode` enabled on your buffer to use `devrel-mode` but
for things like `riak console` support `devrel-mode` uses EDTS
functions so it should be loaded in Emacs. In addition, several of the
functions (specifically the ones that update beam files) assume that
you have compiled the file w/ recent changes (they don't compile for
you). This is made much easier by using EDTS. You must at least be
using `erlang-mode` in order for devrel-mode to start automatically
(it uses the mode's hook).

## Other Caveats

This mode, stupidly, has a hard-coded list of Riak dependencies. The
list, so far, only contains `riak_core` and `riak_kv` (see
[here](https://github.com/jrwest/devrel-mode/blob/master/devrel-mode.el#L41-L45)). This
is only used in the hook that enables `devrel-mode`. These are the
only deps I've worked w/ so far but it should work for others. Extend
it and submit PR after testing (or wait til I do :)).

## Installation

Clone this repo. Add the path to your Emacs load path and `(require
'devrel-mode)`.

## Usage

*devrel-mode* comes with the following built in commands. All commands
 are prefixed with the key binding `C-x C-r`.

* `C-r` - display list of running nodes (pinging each one to
  determine if they are up)
* `s n` - start nodes using `riak start`. will prompt for a list of
  which (dev1 - devN)
* `x n` - stops a node using `riak stop`. will prompt for
  which (dev1 - devN)
* `r n` - restarts a node using `riak stop` then `riak start`. will
  prompt for which (dev1 - devN)
* `C-c` - displays (and possibly starts using `riak console`) the
  console for a node. Will prompt for which (dev1 - devN)
* `m s` - displays member status (using dev1) for the devrel
* `C-x` - resets nodes (stops them and deletes their data directories)
* `c j` - join a node to the cluster. prompts for which node to join
  to which (dev1 - devN).
* `c p` - runs `riak-admin cluster plan` from dev1
* `c c` - runs `riak-admin cluster commit` from dev1
* `c b` - builds a cluster, stopping and resetting any running
  nodes. prompts for which nodes to build the cluster from (dev1 - devN).
* `C-b` - update BEAM for buffer. finds the corresponding BEAM file
  for the Erlang file in the buffer and copies it into the right places
  in the devrel
* `C-a` - update all BEAM files for dependency. Given current buffer
  determine the riak dependency (e.g. riak_core) and update all beam
  files in the devrel nodes for it.

`devrel-mode` works with two buffers: `*msgs devrel-mode*` and
`*devrel-mode*`. The latter is shown to you when necessary but it may
be beneficial to know about them if things go wrong.

# License

See `LICENSE.txt`.
