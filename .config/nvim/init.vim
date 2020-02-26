set nocompatible
filetype plugin indent on

" Plugins will go there eventually
call plug#begin('~/.vim/plugged')

Plug 'tomasr/molokai'
Plug 'preservim/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'vim-scripts/Coq-indent'
Plug 'vim-scripts/coq-syntax'
Plug 'kien/ctrlp.vim'
Plug 'vim-airline/vim-airline'

call plug#end()

" Set up options
set autoindent
set expandtab
set softtabstop=4
set shiftwidth=4
set shiftround
set backspace=indent,eol,start
set hidden
set laststatus=2
set display=lastline
set showmode
set showcmd
set nobackup
set nonumber
set incsearch
set ttyfast
set lazyredraw
set autoread
set ruler
set ignorecase
set smartcase
set go-=m " Hide menu bar
set go-=T " Hide tool bar
set go-=r " Hide scroll bar
set encoding=utf8

set wildmode=longest,list,full
set wildmenu

" Leader and shortcuts
let mapleader = " "
map <Leader><Leader> :w<CR>
map <Leader><Right> :bn<CR>
map <Leader><Left> :bp<CR>
map <Leader>q :wq<CR>
map <Leader>h :set hlsearch! hlsearch?<CR>
map <Leader>r :set rnu! rnu?<CR>
map <Leader>n :set nu! rnu?<CR>
map <Leader>f :CtrlP<CR>
map <Leader>b :CtrlPBuffer<CR>
  
" Theme and font
let g:molokai_original = 1
colorscheme molokai

if has('gui_running')
    set guifont=JetBrains\ Mono\ 12
endif

" CtrlP is waaay faster using this (needs the silver searcher, provinding the
" ag command
if executable('rg')
    set grepprg=rg\ --color=never
    let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
    let g:ctrlp_use_caching = 0
endif   

" Status bar
let g:airline_powerline_fonts = 1
