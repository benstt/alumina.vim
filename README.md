# alumina.vim

## Description

This is a Vim plugin that provides [Alumina][a] file detection and syntax highlighting.

**NOTE**: This plugin is re-arranged based on [rust.vim](https://github.com/rust-lang/rust.vim) and not all Alumina features may be included, so expect some inconsistencies. (And viceversa, you may see Rust highlighting that is not in Alumina.)

## Installation

For activating the full functionality, this plugin requires either a plugin
manager or the `.vimrc` to have the following:

```vim
syntax enable
filetype plugin indent on
```

Most plugin managers don't do this automatically, so these statements are
usually added by users in their `vimrc` _right after_ the plugin manager load
section.

### [Vim8 packages][vim8pack]

```sh
git clone https://github.com/benstt/alumina.vim ~/.vim/pack/plugins/start/alumina.vim
```

### [Vundle][v]

```vim
Plugin 'benstt/alumina.vim'
```

### [Pathogen][p]

```sh
git clone --depth=1 https://github.com/benstt/alumina.vim.git ~/.vim/bundle/alumina.vim
```

### [vim-plug][vp]

```vim
Plug 'benstt/alumina.vim'
```

### [dein.vim][d]

```vim
call dein#add('benstt/alumina.vim')
```

### [NeoBundle][nb]

```vim
NeoBundle 'benstt/alumina.vim'
```

## License

Like Alumina, alumina.vim is primarily distributed under the terms of both the MIT
license and the Apache License (Version 2.0). See LICENSE-APACHE and
LICENSE-MIT for details.

[a]: https://github.com/alumina-lang/alumina
[v]: https://github.com/gmarik/vundle
[p]: https://github.com/tpope/vim-pathogen
[nb]: https://github.com/Shougo/neobundle.vim
[vp]: https://github.com/junegunn/vim-plug
[d]: https://github.com/Shougo/dein.vim
[vim8pack]: http://vimhelp.appspot.com/repeat.txt.html#packages
