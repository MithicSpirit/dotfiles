set nobackup
set noswapfile
set number
set clipboard+=unnamedplus
set ignorecase
set smartcase
set splitbelow
set splitright
set mouse=a
set tabstop=4
set lazyredraw

nnoremap Y y$
nnoremap x "_x

nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-l> <C-w><C-l>
nnoremap <C-h> <C-w><C-h>
nnoremap <C-,> <C-w><
nnoremap <C-.> <C-w>>

command W w
command Q q
command Wq wq
command Wsudo w !sudo tee % > /dev/null
