set nobackup
set noswapfile
set number
set clipboard+=unnamedplus
set ignorecase
set smartcase
set mouse=a
set lazyredraw

set splitbelow
set splitright

set tabstop=4
set softtabstop=4
set shiftwidth=4

nnoremap Y y$
nnoremap x "_x
nnoremap ~ g~l
nnoremap s <NOP>
nnoremap <ESC> :noh<CR>

nnoremap <C-w>, <C-w><
nnoremap <C-w>. <C-w>>

command W w
command Q q
command Wq wq
command Wsudo w !sudo tee % > /dev/null

autocmd VimLeave * set guicursor=a:ver20

call plug#begin()
call plug#end()
