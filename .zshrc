export ZPLUG_HOME=/usr/local/opt/zplug
source $ZPLUG_HOME/init.zsh

zplug "zsh-users/zsh-syntax-highlighting"

zplug mafredri/zsh-async, from:github
zplug sindresorhus/pure, use:pure.zsh, from:github, as:theme
zplug "plugins/autojump", from:oh-my-zsh

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi


zplug load

alias s='git status'

export EDITOR=emacs
alias changes='git add . && git commit -am "Changes" && git push'
alias staged='git diff --staged'

alias disassemble='yaxdis -a x86_64'
setopt share_history

alias -g pair-greg='--trailer "Co-authored-by: Grzegorz Caban <nabacg@gmail.com>"'
setopt share_history
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down

# Be able to search forward
stty -ixon

# make git scroll with scroll wheel
git config --global core.pager "less -+\$LESS -RS"


export PATH="/usr/local/opt/llvm/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/llvm/lib"
export CPPFLAGS="-I/usr/local/opt/llvm/include"
export LLVM_SYS_130_PREFIX="/usr/local/opt/llvm/"

export PATH="/usr/local/opt/openssl@3/bin:$PATH"
export PATH="/usr/local/opt/ruby/bin:$PATH"
export PATH="/Library/Developer/CommandLineTools/Library/PrivateFrameworks/:$PATH"
source /usr/local/opt/chruby/share/chruby/chruby.sh

