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
Plugin 'itchyny/lightline.vim'
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
set ignorecase
set smartcase

autocmd VimResized * :wincmd = "automatically rebalance windows on resize

" Turn on syntax highlinting and line numbering
syntax on
set number
set lazyredraw

filetype plugin indent on

" set system clipboard to the un-named one
set clipboard=unnamedplus

" j and k listen to the current text wrapping in visual mode and normal mode
nnoremap j gj
nnoremap k gk

" remap Y to copy to end of line (as opposed to another way of doing yy)
nnoremap Y y$

if expand('%:p') =~# '/home/*/.vimrc' || expand('%:p') =~# '/home/*/*git*/.vimrc' || expand('%:p') =~# '/home/*/*git*/vimrc' 
	nnoremap <F1> :source %<CR>
endif

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

if has('gui_running')
  set guioptions-=T            "turn off toolbar
  set guioptions-=m            "turn off menubar

  "turn off scrollbars
  set guioptions-=l
  set guioptions-=L
  set guioptions-=r
  set guioptions-=b
end

" Strip the newline from the end of a string
function! Chomp(str)
  return substitute(a:str, '\n$', '', '')
endfunction

" Find a file and pass it to cmd
function! DmenuOpen(cmd)
  let fname = Chomp(system("git ls-files | dmenu -i -l 20 -p " . a:cmd))
  if empty(fname)
    return
  endif
  execute a:cmd . " " . fname
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""Plugins""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""

"Gundo
nnoremap <F6> :GundoToggle<CR>

"""Easymotion
"Remap J & K to leader J K
noremap <Leader>j J
noremap <Leader>k K

map <silent> L <Plug>(easymotion-lineforward)
map <silent> J <Plug>(easymotion-j)
map <silent> K <Plug>(easymotion-k)
map <silent> H <Plug>(easymotion-linebackward)

let g:EasyMotion_keys='aoeuihd,.k'

""""Eclim
"Make eclim and ycm play nice
let g:EclimCompletionMethod = 'omnifunc'


"""Fugative
nnoremap <space>ga :Git add %:p<CR><CR>
nnoremap <space>gs :Gstatus<CR>
nnoremap <space>gb :Gblame<CR>
vnoremap <space>gb :'<,'>Gblame<CR>
nnoremap <space>gc :Gcommit -v -q<CR>
nnoremap <space>gt :Gcommit -v -q %:p<CR>
nnoremap <space>gw :Gwrite<CR><CR>
nnoremap <space>gps :Git push<CR>
nnoremap <space>gpl :Git pull<CR>
nnoremap <space>gd :Gdiff<CR>
nnoremap <space>go :exec DmenuOpen("badd")<CR>
