" =======================
" Plug Calling Initiates
" =======================


call plug#begin('~/.vim/plugged')



" Nerdtree
Plug 'preservim/nerdtree'


" Nerdtree icons
Plug 'ryanoasis/vim-devicons'


" code completion
Plug 'neoclide/coc.nvim', {'branch': 'release'}


" Lightline
"Plug 'itchyny/lightline.vim'

"Plug 'shinchu/lightline-gruvbox.vim'



" Goyo mode
Plug 'junegunn/goyo.vim'


" Limelight mode
Plug 'junegunn/limelight.vim'



" Tagbar
Plug 'preservim/tagbar'


" Gruvbox
Plug 'morhetz/gruvbox'


" css colours
Plug 'ap/vim-css-color'

"JSX vim plugin
Plug 'mxw/vim-jsx'


Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'styled-components/vim-styled-components', { 'branch': 'main' }
Plug 'jparise/vim-graphql'



call plug#end()


" =====================
" Custom Status Line
" =====================

set statusline=
set statusline+=%#NonText#
set statusline+=%=
set statusline+=\ %f
set statusline+=\ 
set statusline+=%#CursorLineNr#
set statusline+=\ %y
set statusline+=\ %r
set statusline+=%#IncSearch#
set statusline+=\ %l/%L
set statusline+=\ [%c]


" =========================
" Variables & Nvim Settings
" =========================

set termguicolors
colorscheme gruvbox
set background=dark
set mouse=a
set encoding=UTF-8
set nobackup
set nowritebackup
set nocursorline
set splitbelow
set splitright
set shiftwidth=4
set autoindent
set smartindent
set tabstop=4
set softtabstop=4
set expandtab
set number
syntax enable
hi Normal guibg=#111111



let g:limelight_conceal_ctermfg = 240
let g:limelight_conceal_guifg = '#777777'

"let g:lightline = {
"      \ 'colorscheme': 'gruvbox',
"      \ }




" =========================
" Custom Vim Functions
" =========================





"Goyo Settings
function! s:goyo_enter()
  set noshowmode
  set noshowcmd
  set scrolloff=999
  Limelight
endfunction

function! s:goyo_leave()
  set showmode
  set showcmd
  set scrolloff=5
  Limelight!
  hi! Normal ctermbg=NONE guibg=NONE 
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()


"NERDTree setup

"Exit Vim if NERDTree is the only window left.
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |
    \ quit | endif

"Changing default NERDTree arrows
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

"KeyBind for NERDTree
nnoremap <F4> :NERDTreeToggle<CR>

"KeyBind for TAGbar
nmap <F8> :TagbarToggle<CR>


let g:user_emmet_leader_key='<Tab>'
let g:user_emmet_settings = {
  \  'javascript.jsx' : {
    \      'extends' : 'jsx',
    \  },
  \}

let g:coc_global_extensions = [
  \ 'coc-tsserver',
  \ 'coc-json',
  \ 'coc-css',
  \  'coc-eslint',
  \  'coc-prettier'
  \ ]



autocmd BufEnter *.{js,jsx,ts,tsx} :syntax sync fromstart
autocmd BufLeave *.{js,jsx,ts,tsx} :syntax sync clear


nnoremap <silent> K :call CocAction('doHover')<CR>


function! s:show_hover_doc()
  call timer_start(500, 'ShowDocIfNoDiagnostic')
endfunction


autocmd CursorHoldI * :call <SID>show_hover_doc()
autocmd CursorHold * :call <SID>show_hover_doc()

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gr <Plug>(coc-references)

nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)
nnoremap <silent> <space>s :<C-u>CocList -I symbols<cr>

nnoremap <silent> <space>d :<C-u>CocList diagnostics<cr>

nmap <leader>do <Plug>(coc-codeaction)

nmap <leader>rn <Plug>(coc-rename)
