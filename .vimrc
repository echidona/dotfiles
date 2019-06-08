set ambiwidth=double
set incsearch
set ignorecase
set mouse=a

nnoremap <silent><Esc><Esc> :<C-u>set nohlsearch!<CR>

set whichwrap=b,s,h,l,<,>,[,],~

if &term =~ "xterm"
    let &t_SI .= "\e[?2004h"
    let &t_EI .= "\e[?2004l"
    let &pastetoggle = "\e[201~"

    function XTermPasteBegin(ret)
        set paste
        return a:ret
    endfunction

    inoremap <special> <expr> <Esc>[200~ XTermPasteBegin("")
endif

set showmatch
source $VIMRUNTIME/macros/matchit.vim

set history=5000
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" remind previous cursor position
if has("autocmd")
    autocmd BufReadPost *
    \ if line("'\"") > 0 && line ("'\"") <= line("$") |
    \   exe "normal! g'\"" |
    \ endif
endif
" enable tab key for autocomplete
set wildmenu
" hilite to search
set hlsearch
" ignore capital or small letter
set smartcase
" syntax
syntax on
" don't show colum number and setting color
set nonumber
highlight LineNr ctermfg=blue

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" autoclosed or appeared pair
" imap { {}<LEFT>
" imap [ []<LEFT>
" imap ( ()<LEFT>
" imap < <><LEFT>
" add key-bind
nnoremap j gj
nnoremap k gk
" tab setting
set expandtab
set tabstop=4
set softtabstop=4
set autoindent
set smartindent
set shiftwidth=4

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" package manager
" begin the section of vim-plug
if has('vim_starting')
  set rtp+=~/.vim/plugged/vim-plug
  if !isdirectory(expand('~/.vim/plugged/vim-plug'))
    echo 'install vim-plug...'
    call system('mkdir -p ~/.vim/plugged/vim-plug')
    call system('git clone https://github.com/junegunn/vim-plug.git ~/.vim/plugged/vim-plug/autoload')
  end
endif

call plug#begin('~/.vim/plugged')
Plug 'Shougo/unite.vim'
Plug 'ujihisa/unite-colorscheme'
" and color-scheme
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" file tree
Plug 'scrooloose/nerdtree'
" comment-region
Plug 'tomtom/tcomment_vim'
" autoclose
Plug 'tpope/vim-surround'

"" For Rust
Plug 'racer-rust/vim-racer'
Plug 'rust-lang/rust.vim'
Plug 'thinca/vim-quickrun'

call plug#end()

set hidden
let g:racer_cmd = '$HOME/.cargo/bin/racer'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-airline setting
let g:airline_theme = 'murmur'
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
set ttimeoutlen=50
" NERDtree setting
map <C-f> :NERDTreeToggle<CR>
" QuickRun setting
map <C-r> :QuickRun
