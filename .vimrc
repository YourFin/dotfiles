set nocp
set rtp+=~/.vim/autoload/pathogen.vim
call pathogen#infect()

" eclim and YCM play nice
let g:EclimCompletionMethod = 'omnifunc'

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

" automatically takes the last search for the first parameter in substitute
" commands, i.e. searching foo
" and then doing
" :s//bar/g
" will replace all foos with bar
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
