language-name = 简体中文

# AUR Packages (-A)
A-install-deps = 正在解析依赖...
A-install-repo-pkgs = 仓库依赖包：
A-install-aur-pkgs = AUR 包：
A-install-path-comp = 提取最终组件失败：{ $path }
A-install-ignored = { $file } 已被标记为忽略。仍要安装吗？

A-build-prep = 正在准备构建目录...
A-build-pkg = 正在构建 { $pkg }...
A-build-diff = 要显示构建文件的差异吗？
A-build-hotedit-pkgbuild = 要编辑 PKGBUILD 吗？
A-build-hotedit-install = 要编辑 .install 文件吗？
A-build-fail = 构建失败。
A-build-e-pkgctl = 在隔离的 chroot 环境内构建失败。
A-build-e-makepkg = makepkg 失败。
A-build-e-edit = 编辑失败：{ $file }
A-build-e-tarball = 移动文件失败：{ $file }
A-build-e-filename = 从以下路径提取文件名失败：{ $file }
A-build-e-copies = 复制构建文件失败。
A-build-e-perm = 为以下目录设置文件权限失败：{ $dir }
A-build-pkglist = 无法从以下目录确定 makepkg 输出路径：{ $dir }
A-build-pull = 未能从 'git pull' 获取最新更新。
A-build-continue = 仍要继续吗？

A-i-repo = 仓库
A-i-version = 版本
A-i-status = AUR 状态
A-i-maintainer = 维护者
A-i-proj-url = 项目 URL
A-i-aur-url = AUR URL
A-i-license = 许可证
A-i-group = 软件包组
A-i-provides = 提供
A-i-depends = 依赖
A-i-make = 构建依赖
A-i-opt = 可选依赖
A-i-check = 检查依赖
A-i-votes = 票数
A-i-pop = 流行度
# description
A-i-desc = 描述
A-i-keywords = 关键词
A-i-submitted = 提交时间
A-i-updated = 更新时间

A-u-fetch-info = 正在获取软件包信息...
A-u-comparing = 正在比较软件包版本...
A-u-no-upgrades = 没有可升级的 AUR 包。
A-u-to-upgrade = 要升级的 AUR 包：
A-u-git = 需要重建的 VCS 包：

A-w = 正在克隆 { $package }...

A-y-refreshing = 正在刷新已知 AUR 包的本地克隆...
A-y-pulling = 正在拉取最新提交

# Snapshots (-B)
B-saved = 软件包状态已保存。
B-clean = 删除过时的快照吗？
B-none = 未找到可用的快照。
B-select = 请选择要恢复的快照：

# Cache (-C)
C-size = 当前缓存大小：{ $size }

C-b-file = { $target } 已存在且不是一个目录。
C-b-nonempty = 目标 { $target } 已存在且不为空！
C-b-target = 正在将缓存备份到 { $target }
C-b-curr = 读取当前目录失败。

C-i-latest = 最新
C-i-created = 创建于
C-i-installed = 已安装
C-i-sig = 签名
C-i-size = 压缩包大小
C-i-avail = 可用版本

C-c-keep = 每个包文件将保留 { $pkgs } 个版本。其余将被删除。
C-c-freed = 已释放 { $bytes }。

C-downgrade-which = 要安装 { $pkg } 的哪个版本？

C-y-no-work = 软件包缓存已同步。
C-t-invalids = 正在移除无效的软件包压缩包。

# Logs (-L)
L-first = 首次安装
L-upgrades = 更新数量
L-recent = 最近活动
L-search-err = 通过 { $cmd } 搜索日志失败。
L-view-err = 无法打开您的 ALPM 日志。

# Opening Pages (open)
open-err = 无法打开 { $url }。

# System Statistics (stats)
stats-local = 加载语言数据失败。
stats-host = 主机
stats-user = 用户
stats-distro = 发行版
stats-editor = 编辑器
stats-pkgs = 已安装的软件包
stats-aura-cache = Aura 软件包缓存
stats-pacman-cache = Pacman 软件包缓存
stats-aura-build = Aura 构建缓存
stats-tmp = /tmp 目录

# System Validation (check)
check-start = 正在验证您的系统。
check-missing-exec = 修复：请安装 { $exec } 并/或确保其在您的 PATH 中。
check-env = 环境
check-env-editor = EDITOR 变量是否已设置？
check-env-editor-exec = EDITOR 值 ({ $exec }) 可执行吗？
check-env-editor-vi = 备用编辑器 vi 可执行吗？
check-env-exec = { $exec } 是否已安装且可执行？
check-env-lang = { $cmd } 是否包含 LANG 值？({ $lang })
check-env-lang-fix = 请更新您的 { $file } 以包含 { $lang }。
check-env-lang-fix2 = 设置您的 LANG 变量！
check-env-lang-known = Aura 是否针对您的 LANG 进行了本地化？
check-env-java-bin = Java 工具是否已安装？
check-env-java-bin-fix = 请考虑安装 { $pkg }。
check-env-java-set = Java 环境是否已设置？
check-env-java-set-fix = 请参阅 { $cmd }。
check-pconf = Pacman 配置 (/etc/pacman.conf)
check-pconf-par = 并行下载是否已激活？
check-pconf-par-fix = { $setting } 已关闭，或设置为 1。请设置 { $set } 以加速压缩包获取。
check-pconf-ignores = 是否有重叠的忽略软件包？
check-pconf-ignores-fix = 以下软件包同时在 pacman.conf 和 aura.toml 中被忽略：{ $pkgs }
check-pconf-pacnew = 是否所有 .pacnew 文件都已处理？
check-pconf-pacnew-broken = 错误：调用 { $fd } 完全失败。

check-pconf-pacnew-old = { $path } 比其 .pacnew 文件旧 { $days ->
    [one] 1 天。
   *[many] {$days} 天。
}

check-aconf = Aura 配置
check-aconf-aura-exists = Aura 配置文件是否存在？
check-aconf-aura-exists-fix = 修复：请考虑执行 { $cmd }
check-aconf-aura-parse = Aura 配置文件可以被解析吗？
check-aconf-old-dirs = 是否存在旧的 Aura 目录？
check-aconf-old-conf = 是否存在旧的 Aura 配置文件？
check-mconf = Makepkg 配置 ({ $path })
check-mconf-packager = PACKAGER 是否已设置？
check-mconf-packager-fix = 修复：请在 { $path } 内设置 { $cmd }
check-snapshots = 软件包快照
check-snapshot-usable = 所有快照都有对应的压缩包吗？
check-snapshot-usable-fix = 修复：您可以使用 { $command } 删除旧的/不可用的快照。
check-cache = 软件包压缩包缓存
check-cache-exists = 所有指定的缓存都存在吗？
check-cache-tarballs = 所有压缩包都有效吗？
check-cache-tarballs-fix = 修复：您可以使用 { $command } 删除无效的压缩包。
check-cache-missing = 是否每个已安装的官方软件包都有压缩包？
check-cache-missing-fix = 修复：您可以使用 { $command } 下载缺失的官方压缩包。
check-cache-missing-for = 是否每个已安装的 AUR 软件包都有压缩包？
check-cache-missing-for-fix = 修复：使用 { $cmd } 查看缺失的软件包并手动重新安装它们。
check-pkgs = 软件包状态
check-pkgs-old = 所有明确安装的非依赖软件包都是最新的吗？
check-pkgs-old-warn = { $pkg } 上次更新于 { $days } 天前。
check-pkgs-empty = 所有软件包克隆都已填充吗？
check-pkgs-empty-fix = 修复：请删除以下目录。

# Thanks
thanks-you = 感谢您使用 Aura。
thanks-colin = Aura 作者 Colin Woodbury，2012 - 2024
thanks-pacman = 感谢 Pacman 和 Arch Linux 团队提供了坚实的基础。
thanks-everyone = 感谢 Aura 的贡献者、捐赠者和用户。
thanks-logo = Aura 徽标由 Cristiano Vitorino 设计。
thanks-translators = Aura 已由以下人员完成本地化：

# Configuration (conf)
conf-toml-err = 序列化当前配置失败。

# Dependencies (deps)
deps-io = 生成依赖关系图失败。

# Runtime Environment
env-missing-editor = 提供的 EDITOR 不在 PATH 中。
env-pconf = 无法解析 pacman.conf 文件。

# Pacman Calls
pacman-external = 调用 pacman 完全失败。
pacman-u = 调用 pacman -U 失败。
pacman-s = 调用 pacman -S 失败。
pacman-misc = 调用 pacman 返回了非零退出代码。

# Aura-specific Directories
dir-mkdir = 创建目录失败：{ $dir }。
dir-home = 无法确定 Aura 的配置目录。
dir-cache = 无法确定 Aura 的缓存目录。

# Dependency Resolution
dep-exist = 软件包 { $pkg } 不存在。
dep-exist-par = 依赖项 { $pkg } (属于 { $par }) 不存在。
dep-graph = 依赖关系图格式有误。
dep-cycle = 检测到循环依赖：{ $cycle }
dep-multi = 依赖解析过程中出现多个错误。

# Git Operations
git-diff = git diff 失败：{ $file }
git-hash = 将 git hash 读入 Rust 失败。
git-pull = git pull 失败：{ $dir }
git-clone = git clone 失败：{ $dir }
git-io = 调用 git 失败。

# Faur Calls
faur-fetch = 调用元数据服务器完全失败：{ $pkg }
faur-unknown = 未知软件包：{ $pkg }
faur-too-many = Faur 返回的结果比预期的多：{ $pkg }

# Common Errors
err-alpm = 无法打开 ALPM 句柄。
err-config-path = 无法确定 Aura 配置文件的路径。
err-curl = CURL 事务失败：{ $err }
err-file-del = 删除失败：{ $file }
err-file-open = 无法打开文件句柄：{ $file }
err-file-write = 写入文件失败：{ $file }
err-json-decode = 从以下地址解码 JSON 失败：{ $url }
err-json-write = 写入 JSON 到以下地址失败：{ $file }
err-mutex = 互斥锁已中毒。
err-pool-create = 创建 ALPM 连接池失败。
err-pool-get = 无法从连接池获取 ALPM 句柄。
err-read-dir = 读取目录失败：{ $dir }
err-srcinfo = 解析 .SRCINFO 失败：{ $file }
err-sudo = 无需使用 sudo 运行 Aura。
err-time-conv = 时间戳转换失败。
err-time-format = 格式化时间字符串失败。
err-user-input = 获取用户输入失败。
err-utf8 = UTF-8 转换失败。
err-write = 无法写入标准输出。

# Common Fields
common-yes = 是
common-no = 否
common-name = 名称
common-done = 完成。
common-no-packages = 未指定软件包。
common-no-work = 无事可做。
common-cancelled = 操作已取消。
common-replace = 您可以删除 { $old }，改用 { $new }。

# Misc.
proceed = 是否继续？
proceed-affirmative = y
proceed-affirmative-alt = Y
proceed-negative = n
