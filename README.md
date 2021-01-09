# vim-erlang-tags

## The idea

`vim-erlang-tags` **creates a tags file** (from Erlang source files), which can
be used by Vim.

When using Exuberant ctags or etags, the generated tags will contain function
names, but there will be no `module:function` tags. This is a problem
because if several functions (in different modules) have the same name, the
text editor will not know which one to jump to.

The idea of this script is to generate `module:function` tags too. This way the
code will be **easier to navigate than with ctags or etags**. The original idea
is from László Lövei.

Since `:` is not a keyword character when editing Erlang files in Vim, this
repository also contains a Vim plugin, which modifies the following normal mode
commands to add `:` to the `iskeyword` option for Erlang files while they are
jumping to the location of the tag that is under the cursor:

```
CTRL-]
g<LeftMouse>
<C-LeftMouse>
g]
g CTRL-]
```

## Installation

<details>
<summary>Vim's built-in package manager</summary>

This is the recommended installation method if you use at least Vim 8 and you
don't use another package manager.

Information about Vim's built-in package manager: [`:help packages`].

Installation steps:

1.  Clone this repository (you can replace `foo` with the directory name of your
    choice):

    ```sh
    $ git clone https://github.com/vim-erlang/vim-erlang-tags.git \
          ~/.vim/pack/foo/start/vim-erlang-tags
    ```

2.  Restart Vim.
</details>

<details>
<summary>Pathogen</summary>

Information about Pathogen: [Pathogen repository].

Installation steps:

1.  Clone this repository:

    ```
    $ git clone https://github.com/vim-erlang/vim-erlang-tags.git \
          ~/.vim/bundle/vim-erlang-tags
    ```

2.  Restart Vim.
</details>

<details>
<summary>Vundle</summary>

Information about Vundle: [Vundle repository].

Installation steps:

1.  Add `vim-erlang-tags` to your plugin list in `.vimrc` by inserting
    the line that starts with `Plugin`:

    ```
    call vundle#begin()
      [...]
      Plugin 'vim-erlang/vim-erlang-tags'
      [...]
    call vundle#end()
    ```

2.  Restart Vim.

3.  Run `:PluginInstall`.
</details>

<details>
  <summary>Vim-Plug</summary>

Information about Vim-Plug: [vim-plug repository].

Installation steps:

1.  Add `vim-erlang-tags` to your plugin list in `.vimrc` by inserting the
    line that starts with `Plug`:

    ```
    call plug#begin()
      [...]
      Plug 'vim-erlang/vim-erlang-tags'
      [...]
    call plug#end()
    ```

2.  Restart Vim.

3.  Run `:PlugInstall`.
</details>

## Usage

Let's say you would like to use tags for your Erlang project.

### Generate tags

First you need to generate the tags.

You can do that either in the command line:

```
$ cd /path/to/my_erlang_project
$ /path/to/vim-erlang-tags/bin/vim_erlang_tags.erl
```

Or within Vim by executing the following command:

```
:ErlangTags
```

Note that for the latter command, the current working directory will be used
(`:help pwd` to find out more).

### Options

#### `g:erlang_tags_ignore`

Add ignore path for tags generation. Use a string or list of strings like:

```
    let g:erlang_tags_ignore = 'rel'
    let g:erlang_tags_ignore = ['rel']
```

Default: doesn't exist.

#### `g:erlang_tags_auto_update`

If exists and set to 1, this plugin will be triggered when an Erlang buffer is
written. Warning: this may cost lots of CPU if you have a large project. Note
that it might not work on Windows.

Default: doesn't exist.

#### `g:erlang_tags_auto_update_current`

If exists and set to 1, this plugin will be triggered when an Erlang buffer is
written. In this case, it will attempt to update only the currently modified
file. As a limitation, this will consider that your `tags` file is on the
current directory, as returned by vim's `getcwd()`.
Note that it might not work on Windows.

Default: doesn't exist.

#### `g:erlang_tags_outfile`

This option specifies the name of the generated tags file. By default, the
output file will be `./tags`.

### Automating generating tags

To keep the tags file up-to-date you can re-run these commands periodically, or
automate the process by creating a commit/checkout hook or a crontab entry.

If you use Git, creating a checkout hook is simple:

```
echo '#!/bin/bash' > .git/hooks/post-checkout
echo '/path/to/vim-erlang-tags/bin/vim_erlang_tags.erl' > .git/hooks/post-checkout
chmod +x .git/hooks/post-checkout
cp -i .git/hooks/post-checkout .git/hooks/post-commit
```

### Make Vim use the tags

Add the following line to your `.vimrc`:

```
:set tags^=/path/to/my_erlang_project/tags
```

This will explicitly add the `tags` file to the list of known tags locations.

Reopen Vim or just execute `:source $MYVIMRC` – now all your function names,
records, macros and file names are available with the Vim tag search commands.

### Using the Vim tag search commands

The few most useful tag search commands are the following:

- `CTRL-]`: jump to the definition of the function/record/macro under the cursor
- `:tj ident`: jump to the definition of `ident` (function/record/macro name)

For more information on those commands, see `:help tagsrch.txt`.

## Contributing

*   Please read the [Contributing][vim-erlang-contributing] section of the
    vim-erlang README.

*   If you modify `vim_erlang_tags.erl`, please update the tests in in the
    vim-erlang repository.

<!-- If you modify the list below, please maintain the order with `:sort i`. -->

[`:help packages`]: https://vimhelp.org/repeat.txt.html#packages
[Pathogen repository]: https://github.com/tpope/vim-pathogen
[vim-erlang-contributing]: https://github.com/vim-erlang/vim-erlang#contributing
[vim-plug repository]: https://github.com/junegunn/vim-plug
[Vundle repository]: https://github.com/VundleVim/Vundle.vim
