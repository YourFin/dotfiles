set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
Plugin 'L9'

"Editing
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'valloric/youcompleteme'
Plugin 'vim-syntastic/syntastic'
Plugin 'easymotion/vim-easymotion'
Plugin 'airblade/vim-gitgutter'
Plugin 'sjl/gundo.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
" eclim and YCM play nice
"""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""End Vundle"""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""

" fix airline
"set lastatus=2

set updatetime=750
" highlight current line
augroup CursorLine
	au!
	au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
	au WinLeave * setlocal nocursorline
augroup END
set hlsearch

" fix something:
" http://stackoverflow.com/questions/16359878/vim-how-to-map-shift-enter
" Get return to kill highlited searchs
autocmd CmdwinEnter * nnoremap <CR> <CR>
autocmd BufReadPost quickfix nnoremap <CR> <CR>
nnoremap <CR> :noh<CR><CR>

map <ScrollWheelUp> <C-Y>
map <ScrollWheelDown> <C-E>

let mapleader = "-"

set incsearch

" Turn on syntax highlinting and line numbering
syntax on
set number

filetype plugin indent on

" set system clipboard to the un-named one
set clipboard=unnamedplus

" j and k listen to the current text wrapping in visual mode and normal mode
nnoremap j gj
nnoremap k gk

" remap Y to copy to end of line (as opposed to another way of doing yy)
nnoremap Y y$


" Delete keys by default going into blackhole register, with 'cut' rebound to m
noremap gm m
noremap m d
noremap mm dd
noremap M D
noremap dd "_dd
noremap x "_x
noremap X "_X
noremap d "_d
noremap D "_D

"""Persistant Undo
" Put plugins and dictionaries in this dir (also on Windows)
let vimDir = '$HOME/.vim'
let &runtimepath.=','.vimDir

" Keep undo history across sessions by storing it in a file
if has('persistent_undo')
    let myUndoDir = expand(vimDir . '/undodir')
    " Create dirs
    call system('mkdir ' . vimDir)
    call system('mkdir ' . myUndoDir)
    let &undodir = myUndoDir
    set undofile
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""Plugins""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""

"Gundo
nnoremap <F6> :GundoToggle<CR>

"""Easymotion
"Remap J & K to leader J K
noremap <Leader>J J
noremap <Leader>K K

map L <Plug>(easymotion-lineforward)
map J <Plug>(easymotion-j)
map K <Plug>(easymotion-k)
map H <Plug>(easymotion-linebackward)

""""Eclim
let g:EclimCompletionMethod = 'omnifunc'
