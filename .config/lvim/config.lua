--[[
lvim is the global options object

Linters should be
filled in as strings with either
a global executable or a path to
an executable
]]
-- THESE ARE EXAMPLE CONFIGS FEEL FREE TO CHANGE TO WHATEVER YOU WANT

-- general
lvim.log.level = "warn"
lvim.format_on_save = true
-- lvim.colorscheme = "onenord"

lvim.colorscheme = "minimal-pro-pro"
-- lvim.colorscheme = "gruvbox-material"
--lvim.colorscheme = "everforest"
-- lvim.colorscheme = "catppuccin"
-- lvim.colorscheme = 'minimal'
vim.g.catppuccin_flavour = "mocha" -- latte, frappe, macchiato, mocha
vim.opt.shiftwidth = 4 -- the number of spaces inserted for each indentation
vim.opt.tabstop = 4 -- insert 4 spaces for a tab
vim.opt.relativenumber = true -- set relative numbered lines
vim.g.code_action_menu_window_border = 'single'
-- to disable icons and use a minimalist setup, uncomment the following
-- lvim.use_icons = false
vim.o.termguicolors = true -- fixes vim off colors when using tmux
-- keymappings [view all the defaults by pressing <leader>Lk]
lvim.leader = "space"
-- add your own keymapping
lvim.keys.normal_mode["<C-s"] = ":w<cr>"
lvim.keys.normal_mode["<leader>a"] = ":CodeActionMenu<cr>"
lvim.keys.normal_mode["<leader> "] = ":RnvimrToggle<cr>"


-- unmap a default keymapping
-- vim.keymap.del("n", "<C-Up>")
-- override a default keymapping
-- lvim.keys.normal_mode["<C-q>"] = ":q<cr>" -- or vim.keymap.set("n", "<C-q>", ":q<cr>" )

-- Change Telescope navigation to use j and k for navigation and n and p for history in both input and normal mode.
-- we use protected-mode (pcall) just in case the plugin wasn't loaded yet.
-- local _, actions = pcall(require, "telescope.actions")
-- lvim.builtin.telescope.defaults.mappings = {
--   -- for input mode
--   i = {
--     ["<C-j>"] = actions.move_selection_next,
--     ["<C-k>"] = actions.move_selection_previous,
--     ["<C-n>"] = actions.cycle_history_next,
--     ["<C-p>"] = actions.cycle_history_prev,
--   },
--   -- for normal mode
--   n = {
--     ["<C-j>"] = actions.move_selection_next,
--     ["<C-k>"] = actions.move_selection_previous,
--   },
-- }

-- Use which-key to add extra bindings with the leader-key prefix
lvim.builtin.which_key.mappings["P"] = {
    "<cmd>Telescope projects<CR>",
    "Projects"
}
lvim.builtin.which_key.mappings["t"] = {
    name = "+Trouble",
    r = { "<cmd>Trouble lsp_references<cr>", "References" },
    f = { "<cmd>Trouble lsp_definitions<cr>", "Definitions" },
    d = { "<cmd>Trouble document_diagnostics<cr>", "Diagnostics" },
    q = { "<cmd>Trouble quickfix<cr>", "QuickFix" },
    l = { "<cmd>Trouble loclist<cr>", "LocationList" },
    w = { "<cmd>Trouble workspace_diagnostics<cr>", "Wordspace Diagnostics" },
}
lvim.builtin.which_key.mappings["r"] = {

    name = "Rust",
    a = { "<cmd>RustCodeAction<Cr>", "Code Action" },
    h = { "<cmd>RustToggleInlayHints<Cr>", "Toggle Hints" },
    r = { "<cmd>RustRunnables<Cr>", "Runnables" },
    -- r = { "<cmd>lua _CARGO_RUN()<cr>", "Cargo Run" },
    t = { "<cmd>lua _CARGO_TEST()<cr>", "Cargo Test" },
    m = { "<cmd>RustExpandMacro<Cr>", "Expand Macro" },
    o = { "<cmd>RustOpenCargo<Cr>", "Open Cargo" },
    p = { "<cmd>RustParentModule<Cr>", "Parent Module" },
    -- j = { "<cmd>RustJoinLines<Cr>", "Join Lines" },
    -- s = { "<cmd>RustStartStandaloneServerForBuffer<Cr>", "Start Server Buf" },
    d = { "<cmd>RustDebuggables<Cr>", "Debuggables" },
    v = { "<cmd>RustViewCrateGraph<Cr>", "View Crate Graph" },
    R = {
        "<cmd>lua require('rust-tools/workspace_refresh')._reload_workspace_from_cargo_toml()<Cr>",
        "Reload Workspace",
    },
}

lvim.builtin.which_key.mappings["2"] = {

    name = "Cargo",
    -- u = { "<cmd>require('crates').update()<CR>", "Toggle Hints" },
    -- r = { "<cmd>require('crates').reload()<CR>", "Toggle Hints" },
    -- Visual
    -- U = { "<cmd>require ('crates').update_crates()<CR>", "" },
    -- G = { "<cmd>require('crates').upgrade_crates()<CR>", "" },
    t = { "<cmd>lua require('crates').toggle()<CR>", "Toggle Hints" },

    u = { "<cmd>lua require('crates').update_crate()<CR>", "Update" },
    U = { "<cmd>lua require('crates').upgrade_crate()<CR>", "Upgrade" },

    a = { "<cmd>lua require('crates').update_all_crates()<CR>", "Update All" },
    A = { "<cmd>lua require('crates').upgrade_all_crates()<CR>", "Upgrade All" },

    h = { "<cmd>lua require('crates').open_homepage()<CR>", "Open Home" },
    r = { "<cmd>lua require('crates').open_repository()<CR>", "Open Repo" },
    d = { "<cmd>lua require('crates').open_documentation()<CR>", "Open Doc" },
    c = { "<cmd>lua require('crates').open_crates_io()<CR>", "Open Crates.io" },
    i = { "<cmd>lua require('crates').show_popup()<CR>", "Info" },
    v = { "<cmd>lua require('crates').show_versions_popup()<CR>", "Versions" },
    f = { "<cmd>lua require('crates').show_features_popup()<CR>", "Features" },
    D = { "<cmd>lua require('crates').show_dependencies_popup()<CR>", "Dependencies" },
}
-- TODO: User Config for predefined plugins
-- After changing plugin config exit and reopen LunarVim, Run :PackerInstall :PackerCompile
lvim.builtin.alpha.active = true
lvim.builtin.notify.active = false
lvim.builtin.terminal.active = true
lvim.builtin.nvimtree.setup.view.side = "left"
lvim.builtin.nvimtree.setup.renderer.icons.show.git = true
lvim.builtin.lualine.active = true
lvim.builtin.lualine.style = "default"
-- vim.g.minimal_italic_functions = true
-- vim.g.minimal_italic_keywords = true
-- vim.g.minimal_italic_comments = true

-- +-------------------------------------------------+
-- | A | B | C                             X | Y | Z |
-- +-------------------------------------------------+
-- insert -> insert mode, normal -> normal mode, visual -> visual mode
local custom_16 = require 'lualine.themes.base16'
-- Change the background of lualine_c section for normal mode
custom_16.normal.c.bg = '#1A191E'
custom_16.normal.b.bg = '#1A191E'
--custom_16.normal.a.bg = '#8BB8D0'
custom_16.normal.c.fg = '#695F69'
custom_16.normal.b.fg = '#695F69'
custom_16.insert.b.fg = '#695F69'
custom_16.insert.b.bg = '#1A191E'
custom_16.visual.b.bg = '#1A191E'
custom_16.insert.b.fg = '#695F69'
custom_16.visual.b.fg = '#695F69'
custom_16.insert.a.bg = '#A896BE'

lvim.builtin.lualine.options = {
    icons_enabled = true,
    -- theme = 'onedark',
    -- theme = "nord"
    theme = custom_16
}




-- lvim.builtin.lualine.options.theme = 'base16'
-- lvim.builtin.lualine.options.theme = 'gruvbox'
-- lvim.builtin.lualine.options.theme = 'everforest'



-- gitsigns overriden configs
lvim.builtin.gitsigns.opts.signs = {

    add          = { hl = 'GitSignsAdd', text = '+', numhl = 'GitSignsAddNr', linehl = 'GitSignsAddLn' },
    change       = { hl = 'GitSignSChange', text = '+-', numhl = 'GitSignsChangeNr', linehl = 'GitSignsChangeLn' },
    delete       = { hl = 'GitSignsDelete', text = '✖', numhl = 'GitSignsDeleteNr', linehl = 'GitSignsDeleteLn' },
    topdelete    = { hl = 'GitSignsDelete', text = '‾', numhl = 'GitSignsDeleteNr', linehl = 'GitSignsDeleteLn' },
    changedelete = { hl = 'GitSignsChange', text = '~-', numhl = 'GitSignsChangeNr', linehl = 'GitSignsChangeLn' },

}


-- toggleterm configs
lvim.builtin.terminal.direction = "horizontal"
lvim.builtin.terminal.shading_factor = -6
lvim.builtin.terminal.open_mapping = "<C-j>"
-- diable arrow keys in normal mode
-- vim.cmd([[
--   	map <up> <nop>
--   	map <down> <nop>
--   	map <left> <nop>
--   	map <right> <nop>
-- ]])

-- -- diable arrow keys in insert mode
-- vim.cmd([[
--   	imap <up> <nop>
-- 	imap <down> <nop>
-- 	imap <left> <nop>
-- 	imap <right> <nop>
-- ]])

vim.cmd([[
set background=dark
set t_Co=256
if !has("gui_running")
    set t_Co=256
endif
]])

vim.cmd([[
let g:code_action_menu_show_diff = v:false
]])

-- if you don't want all the parsers change this to a table of the ones you want
lvim.builtin.treesitter.ensure_installed = {
    "bash",
    "c",
    "javascript",
    "json",
    "lua",
    "markdown",
    "vim",
    "python",
    "typescript",
    "tsx",
    "css",
    "rust",
    "java",
    "yaml",
    "go",
    "cpp",
}

lvim.builtin.treesitter.ignore_install = { "haskell" }
lvim.builtin.treesitter.highlight.enabled = true
lvim.builtin.alpha.mode = "startify"
lvim.builtin.alpha.startify.section.header.val = {

    "                                                             ",
    "  ##    ##  ########  #######   ##     ##  ####  ##     ##   ",
    "  ##    ##  ##       ##     ##  ##     ##   ##   ###   ###   ",
    "  ###   ##  ##       ##     ##  ##     ##   ##   #### ####   ",
    "  ## ## ##  ######   ##     ##  ##     ##   ##   ## ### ##   ",
    "  ##  ####  ##       ##     ##   ##   ##    ##   ##     ##   ",
    "  ##   ###  ##       ##     ##    ## ##     ##   ##     ##   ",
    "  ##    ##  ########  #######      ###     ####  ##     ##   ",
    "                                                             ",
    "      011011100110010101101111011101100110100101101101       ",
}
-- ---@usage disable automatic installation of servers
-- lvim.lsp.automatic_servers_installation = false

-- ---configure a server manually. !!Requires `:LvimCacheReset` to take effect!!
-- ---see the full default list `:lua print(vim.inspect(lvim.lsp.automatic_configuration.skipped_servers))`
-- vim.list_extend(lvim.lsp.automatic_configuration.skipped_servers, { "pyright" })
-- local opts = {} -- check the lspconfig documentation for a list of all possible options
-- require("lvim.lsp.manager").setup("pyright", opts)

-- ---remove a server from the skipped list, e.g. eslint, or emmet_ls. !!Requires `:LvimCacheReset` to take effect!!
-- ---`:LvimInfo` lists which server(s) are skiipped for the current filetype
-- vim.tbl_map(function(server)
--   return server ~= "emmet_ls"
-- end, lvim.lsp.automatic_configuration.skipped_servers)

-- -- you can set a custom on_attach function that will be used for all the language servers
-- -- See <https://github.com/neovim/nvim-lspconfig#keybindings-and-completion>
-- lvim.lsp.on_attach_callback = function(client, bufnr)
--   local function buf_set_option(...)
--     vim.api.nvim_buf_set_option(bufnr, ...)
--   end
--   --Enable completion triggered by <c-x><c-o>
--   buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")
-- end

-- -- set a formatter, this will override the language server formatting capabilities (if it exists)
-- local formatters = require "lvim.lsp.null-ls.formatters"
-- formatters.setup {
--   { command = "black", filetypes = { "python" } },
--   { command = "isort", filetypes = { "python" } },
--   {
--     -- each formatter accepts a list of options identical to https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md#Configuration
--     command = "prettier",
--     ---@usage arguments to pass to the formatter
--     -- these cannot contain whitespaces, options such as `--line-width 80` become either `{'--line-width', '80'}` or `{'--line-width=80'}`
--     extra_args = { "--print-with", "100" },
--     ---@usage specify which filetypes to enable. By default a providers will attach to all the filetypes it supports.
--     filetypes = { "typescript", "typescriptreact" },
--   },
-- }

-- -- set additional linters
-- local linters = require "lvim.lsp.null-ls.linters"
-- linters.setup {
--   { command = "flake8", filetypes = { "python" } },
--   {
--     -- each linter accepts a list of options identical to https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md#Configuration
--     command = "shellcheck",
--     ---@usage arguments to pass to the formatter
--     -- these cannot contain whitespaces, options such as `--line-width 80` become either `{'--line-width', '80'}` or `{'--line-width=80'}`
--     extra_args = { "--severity", "warning" },
--   },
--   {
--     command = "codespell",
--     ---@usage specify which filetypes to enable. By default a providers will attach to all the filetypes it supports.
--     filetypes = { "javascript", "python" },
--   },
-- }
vim.list_extend(lvim.lsp.automatic_configuration.skipped_servers, { "rust_analyzer" })
require 'lspconfig'.rust_analyzer.setup {
    settings = {
        ["rust-analyzer"] = {
            imports = {
                granularity = {
                    group = "module",
                },
                prefix = "self",
            },
            cargo = {
                buildScripts = {
                    enable = true,
                },
            },
            procMacro = {
                enable = true
            },
        }
    }
}

-- Additional Plugins
lvim.plugins = {

    -- themes
    {

        "yazeed1s/minimal.nvim",
        "sainnhe/gruvbox-material",
        'sainnhe/everforest',
        "w0ng/vim-hybrid",
        "LunarVim/Colorschemes",
        {
            "catppuccin/nvim",
            as = "catppuccin"
        }
    },

    -- code action +
    { 'weilbith/nvim-code-action-menu' },


    -- indentline
    {
        "lukas-reineke/indent-blankline.nvim",
        event = "BufRead",
        setup = function()
            vim.g.indentLine_enabled = 1
            vim.g.indent_blankline_char = ":"
            vim.g.indent_blankline_filetype_exclude = { "help", "terminal", "dashboard" }
            vim.g.indent_blankline_buftype_exclude = { "terminal" }
            vim.g.indent_blankline_show_trailing_blankline_indent = false
            vim.g.indent_blankline_show_first_indent_level = false
        end
    },
    {
        'sudormrfbin/cheatsheet.nvim',
        requires = {
            { 'nvim-telescope/telescope.nvim' },
            { 'nvim-lua/popup.nvim' },
            { 'nvim-lua/plenary.nvim' },
        },
    },
    {
        "folke/trouble.nvim",
        cmd = "TroubleToggle",
    },

    {
        'hood/popui.nvim'
    },

    { "oberblastmeister/neuron.nvim" },

    -- {
    --     "kevinhwang91/rnvimr",
    --     cmd = "RnvimrToggle",
    --     config = function()
    --         vim.g.rnvimr_draw_border = 1
    --         vim.g.rnvimr_pick_enable = 1
    --         vim.g.rnvimr_bw_enable = 1
    --     end,
    -- },

    {
        "kevinhwang91/nvim-bqf",
        event = { "BufRead", "BufNew" },
        config = function()
            require("bqf").setup({
                auto_enable = true,
                preview = {
                    win_height = 12,
                    win_vheight = 12,
                    delay_syntax = 80,
                    border_chars = {
                        "┃", "┃", "━", "━", "┏", "┓", "┗", "┛", "█"
                    },
                },
                func_map = {
                    vsplit = "",
                    ptogglemode = "z,",
                    stoggleup = "",
                },
                filter = {
                    fzf = {
                        action_for = {
                            ["ctrl-s"] = "split"
                        },
                        extra_opts = {
                            "--bind",
                            "ctrl-o:toggle-all",
                            "--prompt", "> "
                        },
                    },
                },
            })
        end,
    },


    {
        "tzachar/cmp-tabnine",
        run = "./install.sh",
        requires = "hrsh7th/nvim-cmp",
        event = "InsertEnter",
    },

    {
        "ray-x/lsp_signature.nvim",
        event = "BufRead",
        config = function() require "lsp_signature".on_attach() end,
    },

    {
        "simrat39/symbols-outline.nvim",
        cmd = "SymbolsOutline",
    },

    {
        "Pocco81/AutoSave.nvim",
        config = function()
            require("autosave").setup()
        end,
    },

    {
        "felipec/vim-sanegx",
        event = "BufRead",
    },
    {
        "npxbr/glow.nvim",
        ft = { "markdown" }
        -- run = "yay -S glow"
    },

    {
        "tpope/vim-surround",

        -- make sure to change the value of `timeoutlen` if it's not triggering correctly, see https://github.com/tpope/vim-surround/issues/117
        -- setup = function()
        --  vim.o.timeoutlen = 500
        -- end
    },


    {
        "folke/todo-comments.nvim",
        event = "BufRead",
        config = function()
            require("todo-comments").setup()
        end,
    },


    {
        "simrat39/rust-tools.nvim",
        config = function()
            local lsp_installer_servers = require "nvim-lsp-installer.servers"
            local _, requested_server = lsp_installer_servers.get_server "rust_analyzer"
            require("rust-tools").setup({
                tools = {
                    autoSetHints = true,
                    hover_with_actions = true,
                    runnables = {
                        use_telescope = true,
                    },
                },
                server = {
                    cmd_env = requested_server._default_options.cmd_env,
                    on_attach = require("lvim.lsp").common_on_attach,
                    on_init = require("lvim.lsp").common_on_init,
                },
                -- options same as lsp hover / vim.lsp.util.open_floating_preview()
                hover_actions = {
                    -- the border that is used for the hover window
                    -- see vim.api.nvim_open_win()
                    border = {
                        { "╭", "FloatBorder" },
                        { "─", "FloatBorder" },
                        { "╮", "FloatBorder" },
                        { "│", "FloatBorder" },
                        { "╯", "FloatBorder" },
                        { "─", "FloatBorder" },
                        { "╰", "FloatBorder" },
                        { "│", "FloatBorder" },
                    },
                    -- whether the hover action window gets automatically focused
                    -- default: false
                    auto_focus = false,
                },

            })
        end,
        ft = { "rust", "rs" },
    },
}

-- C/C++
-- local formatters = require "lvim.lsp.null-ls.formatters"
-- formatters.setup { { exe = "clang-format", args = {} } }
-- some settings can only passed as commandline flags `clangd --help`
local clangd_flags = {
    "--all-scopes-completion",
    "--suggest-missing-includes",
    "--background-index",
    "--pch-storage=disk",
    "--cross-file-rename",
    "--log=info",
    "--completion-style=detailed",
    "--enable-config", -- clangd 11+ supports reading from .clangd configuration file
    "--clang-tidy",
    -- "--clang-tidy-checks=-*,llvm-*,clang-analyzer-*,modernize-*,-modernize-use-trailing-return-type",
    -- "--fallback-style=Google",
    -- "--header-insertion=never",
    -- "--query-driver=<list-of-white-listed-complers>"
}

local clangd_bin = "clangd"

local custom_on_attach = function(client, bufnr)
    require("lvim.lsp").common_on_attach(client, bufnr)
    local opts = { noremap = true, silent = true }
    vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>lh", "<Cmd>ClangdSwitchSourceHeader<CR>", opts)
end

local opts = {
    cmd = { clangd_bin, unpack(clangd_flags) },
    on_attach = custom_on_attach,
}

require("lvim.lsp.manager").setup("clangd", opts)
vim.list_extend(lvim.lsp.automatic_configuration.skipped_servers, { "clangd" })


-- Autocommands (https://neovim.io/doc/user/autocmd.html)
-- vim.api.nvim_create_autocmd("BufEnter", {
--   pattern = { "*.json", "*.jsonc" },
--   -- enable wrap mode for json files only
--   command = "setlocal wrap",
-- })
-- vim.api.nvim_create_autocmd("FileType", {
--   pattern = "zsh",
--   callback = function()
--     -- let treesitter use bash highlight for zsh files as well
--     require("nvim-treesitter.highlight").attach(0, "bash")
--   end,
-- })
