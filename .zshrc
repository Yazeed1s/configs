export ZSH="$HOME/.oh-my-zsh"
export PATH="$HOME/scripts:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"
export QT_QPA_PLATFORMTHEME=qt5ct
export INSTALL4J_JAVA_HOME="/usr/lib/jvm/java-19-openjdk:$PATH"
export PATH="$HOME/.emacs.d/doom/bin:$PATH"
# export PATH="$PATH:$HOME/.local/bin"
export PATH="$HOME/.local/bin:$PATH"
export PATH=/usr/bin:$PATH
export EDITOR=nvim
export VISUAL="$EDITOR"
export QT_QPA_PLATFORMTHEME=qt5ct
export INSTALL4J_JAVA_HOME="/usr/lib/jvm/java-19-openjdk:$PATH"
export MY_INSTALL_DIR=$HOME/.local
# export PATH="$MY_INSTALL_DIR/bin:$PATH"
neofetch
# bsp-layout set even

alias clr='clear'
alias fzv='fzv.sh'
alias fdf='find_.sh'
alias fz='fzf --print0 | xargs -0 -o nvim'
alias grep='grep -n --color'
alias zn='nvim ~/.zshrc'
alias zs='source ~/.zshrc'
alias hg='history | grep'
alias h='history'
alias co='codium .'
alias findf='find . -type f'
alias findd='find . -type d'
#git
alias gitc='git clone'
alias gs='git status'
alias ga='git add'
alias gp='git pull'
alias gd='git diff'
alias gc='git commit -m'
alias gpo='git push origin'
#pacman & yay
alias yup='yay -Syu'
alias pup='sudo pacman -Syu'
alias pcup='sudo pacman -R $(pacman -Qtdq)' # one day this is gonna break my system
alias pc='sudo pacman -Sc'
#disk & memory info
alias mem='free -h'
alias disk='df -h'
#
alias off='sudo shutdown -h now'
# network
alias nt='nmtui'
alias ntl='nmcli device wifi list'
alias ntc='nmcli device wifi connect'
alias nts='nmcli connection show'
alias ntd='nmcli device'
# file transf
alias rs='rsync -avz'
alias scp='scp -r'
#Go
alias gob='go build'
alias goi='go install'
alias gor='go run'
alias got='go test ./...'
alias god='go get -u'
alias goinit='go mod init'
alias gorv='go run -v'

ZSH_THEME="amuse"
plugins=(git)

source $ZSH/oh-my-zsh.sh

source <(ng completion script)
