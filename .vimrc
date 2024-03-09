syntax on
set number
" set cursorline
set shiftwidth=4
set tabstop=4
set expandtab
set nobackup
set showcmd
set nowrap
set incsearch
set ignorecase
set smartcase
set showcmd
set showmode
set showmatch
set hlsearch
set wildmenu
set wildmode=list:longest
set ruler
set statusline=
set statusline+=\ %F\ %M\ %Y\ %R
set statusline+=%=
set statusline+=\ ascii:\ %b\ hex:\ 0x%B\ row:\ %l\ col:\ %c\ percent:\ %p%%
set laststatus=2
set autoindent
set smartindent
set hidden
set ttyfast

set background=dark
set termguicolors

hi clear
let s:t_Co = has('gui_running') ? -1 : (&t_Co ?? 0)
hi! link Terminal Normal
hi! link LineNrAbove LineNr
hi! link LineNrBelow LineNr
hi! link CurSearch Search
hi! link CursorLineFold CursorLine
hi! link CursorLineSign CursorLine
hi! link MessageWindow Pmenu
hi! link PopupNotification Todo
hi Normal guifg=#dac7a6 guibg=NONE gui=NONE cterm=NONE
hi EndOfBuffer guifg=#5f87d7 guibg=NONE gui=NONE cterm=NONE
hi StatusLine guifg=#dac7a6 guibg=#1e1e1e gui=NONE cterm=NONE
hi StatusLineNC guifg=#dac7a6 guibg=#1e1e1e gui=NONE cterm=NONE
hi StatusLineTerm guifg=#dac7a6 guibg=#1e1e1e gui=NONE cterm=NONE
hi StatusLineTermNC guifg=#dac7a6 guibg=#1e1e1e gui=NONE cterm=NONE
hi VertSplit guifg=#dac7a6 guibg=#1e1e1e gui=NONE cterm=NONE
hi PmenuSel guifg=#262626 guibg=#d7d787 gui=NONE cterm=NONE
hi Pmenu guifg=NONE guibg=#4a4a4a gui=NONE cterm=NONE
hi PmenuSbar guifg=NONE guibg=#262626 gui=NONE cterm=NONE
hi PmenuThumb guifg=NONE guibg=#ffd700 gui=NONE cterm=NONE
hi TabLineSel guifg=#000000 guibg=#afaf87 gui=NONE cterm=NONE
hi TabLine guifg=#666666 guibg=#333333 gui=NONE cterm=NONE
hi TabLineFill guifg=#ff8787 guibg=#333333 gui=NONE cterm=NONE
hi ToolbarLine guifg=NONE guibg=NONE gui=NONE ctermfg=NONE ctermbg=NONE cterm=NONE
hi ToolbarButton guifg=#262626 guibg=#d7d787 gui=NONE cterm=NONE
hi NonText guifg=#5f87d7 guibg=NONE gui=NONE cterm=NONE
hi SpecialKey guifg=#ea6962 guibg=NONE gui=NONE cterm=NONE
hi Keyword guifg=#ea6962 guibg=NONE gui=NONE cterm=NONE
hi Conditional guifg=#ea6962 guibg=NONE gui=NONE cterm=NONE
hi Repeat guifg=#ea6962 guibg=NONE gui=NONE cterm=NONE
hi Statement guifg=#ea6962 guibg=NONE gui=NONE cterm=NONE
hi QuickFixLine guifg=#000000 guibg=#5f87d7 gui=NONE cterm=NONE
hi Folded guifg=#666666 guibg=#000000 gui=NONE cterm=NONE
hi FoldColumn guifg=#5f87d7 guibg=#000000 gui=NONE cterm=NONE
hi CursorLine guifg=NONE guibg=#333333 gui=NONE cterm=NONE
hi CursorColumn guifg=NONE guibg=#333333 gui=NONE cterm=NONE
hi ColorColumn guifg=NONE guibg=#000000 gui=NONE cterm=NONE
hi CursorLineNr guifg=NONE guibg=#333333 gui=NONE cterm=NONE
hi Visual guifg=NONE guibg=#442e2d gui=NONE cterm=NONE
hi SignColumn guifg=NONE guibg=#262626 gui=NONE cterm=NONE
hi VisualNOS guifg=#d7d787 guibg=#5f8700 gui=NONE cterm=NONE
hi LineNr guifg=#928374 guibg=NONE gui=NONE cterm=NONE
hi Error guifg=#ea6962 guibg=#ffffff gui=reverse cterm=reverse
hi ErrorMsg guifg=#ff0000 guibg=#000000 gui=reverse cterm=reverse
hi ModeMsg guifg=#928374 guibg=NONE gui=NONE cterm=NONE
hi WarningMsg guifg=#ff8787 guibg=NONE gui=NONE cterm=NONE
hi MoreMsg guifg=#00875f guibg=NONE gui=NONE cterm=NONE
hi Question guifg=#ffd700 guibg=NONE gui=NONE cterm=NONE
hi MatchParen guifg=#000000 guibg=#ffd700 gui=NONE cterm=NONE
hi Search guifg=#000000 guibg=#d7875f gui=NONE cterm=NONE
hi IncSearch guifg=#928374 guibg=#442e2d gui=NONE cterm=NONE
hi Todo guifg=#ff0000 guibg=#ffff00 gui=NONE cterm=NONE
hi WildMenu guifg=#262626 guibg=#d7d787 gui=NONE cterm=NONE
hi Underlined guifg=#5f87d7 guibg=NONE gui=underline cterm=underline
hi Cursor guifg=#333333 guibg=#d7d787 gui=NONE cterm=NONE
hi lCursor guifg=#262626 guibg=#ffafaf gui=NONE cterm=NONE
hi SpellBad guifg=#ff0000 guibg=NONE guisp=#ff0000 gui=undercurl cterm=underline
hi SpellCap guifg=#ffff00 guibg=NONE guisp=#ffff00 gui=undercurl cterm=underline
hi SpellLocal guifg=#ffafaf guibg=NONE guisp=#ffafaf gui=undercurl cterm=underline
hi SpellRare guifg=#ffd7af guibg=NONE guisp=#ffd7af gui=undercurl cterm=underline
hi Comment guifg=#666666 guibg=NONE gui=NONE cterm=NONE
hi String guifg=#d8a657 guibg=NONE gui=NONE cterm=NONE
hi Identifier guifg=#ea6962 guibg=NONE gui=NONE cterm=NONE
hi Function guifg=#a9b665 guibg=NONE gui=NONE cterm=NONE
hi Special guifg=#d3869b guibg=NONE gui=NONE cterm=NONE
hi Constant guifg=#d3869b guibg=NONE gui=NONE cterm=NONE
hi PreProc guifg=#d3869b guibg=NONE gui=NONE cterm=NONE
hi Type guifg=#83a598 guibg=NONE gui=NONE cterm=NONE
hi Operator guifg=#ea6962 guibg=NONE gui=NONE cterm=NONE
hi Define guifg=#d3869b guibg=NONE gui=NONE cterm=NONE
hi Structure guifg=#83a598  guibg=NONE gui=NONE cterm=NONE
hi Directory guifg=#83a598 guibg=NONE gui=bold cterm=bold
hi Conceal guifg=#666666 guibg=NONE gui=NONE cterm=NONE
hi Ignore guifg=NONE guibg=NONE gui=NONE ctermfg=NONE ctermbg=NONE cterm=NONE
hi Title guifg=#ffd700 guibg=NONE gui=bold cterm=bold
hi DiffAdd guifg=#ffffff guibg=#5f875f gui=NONE cterm=NONE
hi DiffChange guifg=#ffffff guibg=#5f87af gui=NONE cterm=NONE
hi DiffText guifg=#000000 guibg=#c6c6c6 gui=NONE cterm=NONE
hi DiffDelete guifg=#ffffff guibg=#af5faf gui=NONE cterm=NONE
