language-name = Tiếng Việt

# AUR Packages (-A)
A-install-deps = Xác định các gói phụ thuộc...
A-install-repo-pkgs = Các repo phụ thuộc:
A-install-aur-pkgs = Các gói AUR:
A-install-path-comp = Thất bại khi giải nén thành phần cuối của: { $path }
A-install-ignored = { $file } được đánh dấu là Bỏ Qua. Tiếp tục cài đặt?

A-build-prep = Đang chuẩn bị thư mục build...
A-build-pkg = Đang build { $pkg }...
A-build-diff = So sánh tệp build?
A-build-hotedit-pkgbuild = Chỉnh sửa PKGBUILD?
A-build-hotedit-install = Bạn có muốn chỉnh sửa tệp .install?
A-build-fail = Build thất bại.
A-build-e-pkgctl = Buid trong môi trường chroot biệt lập thất bại.
A-build-e-makepkg = Có lỗi khi makepkg.
A-build-e-edit = Chỉnh sửa thất bại: { $file }
A-build-e-tarball = Di chuyển thất bại: { $file }
A-build-e-filename = Trích xuất tên tệp thất bại: { $file }
A-build-e-copies = Sao chép tệp build thất bại.
A-build-e-perm = Thiết lập quyền của tệp thất bại: { $dir }
A-build-pkglist = Xác định đường dẫn output của makepkg thất bại: { $dir }
A-build-pull = Thất bại khi dùng 'git pull' để cập nhật.
A-build-continue = Bạn có muốn tiếp tục không?

# A-i-repo = Repository
A-i-version = Phiên bản
A-i-status = Trạng thái AUR
# A-i-maintainer = Maintainer
A-i-proj-url = URL của Dự án
A-i-aur-url = URL của AUR
A-i-license = Bản quyền
A-i-group = Nhóm
A-i-provides = Cung cấp
A-i-depends = Phụ thuộc
A-i-make = Make Phụ thuộc
A-i-opt = Gói Tuỳ chọn
A-i-check = Check Phụ thuộc
# A-i-votes = Votes
A-i-pop = Phổ biến
A-i-desc = Mô tả
A-i-keywords = Từ khoá
A-i-submitted = Khởi tạo
A-i-updated = Cập nhật

A-u-fetch-info = Cập nhật thông tin của gói...
A-u-comparing = So sánh phiên bản của gói...
A-u-no-upgrades = Không có cập nhật cho các gói AUR.
A-u-to-upgrade = Cập nhật các gói AUR:
A-u-git = Các gói VCS được rebuild:

# A-w = Cloning { $package }...

A-y-refreshing = Cập nhật các bản sao cục bộ các gói AUR...
A-y-pulling = Lấy các commit mới nhất

# Snapshots (-B)
B-saved = Đã lưu trạng thái gói.
B-clean = Xoá bỏ các bản lưu trữ cũ?
B-none = Không tìm thấy bản lưu trữ nào.
B-select = Chọn bản lưu trữ để khôi phục:

# Cache (-C)
C-size = Kích cỡ cache hiện tại: { $size }

C-b-file = { $target } đã tồn tại và không phải thư mục.
C-b-nonempty = { $target } đã tồn tại nhưng không phải rỗng!
C-b-target = Sao lưu cache vào { $target }
C-b-curr = Đọc thư mục hiện tại thất bại.

C-i-latest = Mới nhất
C-i-created = Đã tạo
C-i-installed = đã cài
C-i-sig = Chữ ký
C-i-size = Kích cỡ Tarball
C-i-avail = Phiên bản có sẵn

C-c-keep = Sẽ giữ lại { $pkgs } tệp của gói. Phần còn lại sẽ xoá bỏ.
C-c-freed = Giải phóng { $bytes }.

C-downgrade-which = Lựa chọn phiên bản của { $pkg }?

C-y-no-work = Bộ nhớ cache của gói đã đồng bộ.
C-t-invalids = Xoá các tarball không hợp lệ.

# Logs (-L)
L-first = Cài đặt lần đầu
L-upgrades = Cập nhật
L-recent = Hoạt động gần nhất
L-search-err = Tìm kiếm log bằng { $cmd } thất bại.
L-view-err = Mở log của ALPM thất bại.

# Opening Pages (open)
# open-err = Mở { $url } thất bại.

# System Statistics (stats)
stats-local = Thất bại khi tải dữ liệu ngôn ngữ.
# stats-host = Host
# stats-user = User
stats-distro = Bản phân phối
# stats-editor = Editor
stats-pkgs = Gói đã cài
stats-aura-cache = Cache các gói Aura
stats-pacman-cache = Cache các gói Pacman
stats-aura-build = Cache các gói Aura Build
stats-tmp = Thư mục /tmp

# System Validation (check)
check-start = Kiểm tra hệ thống của bạn.
check-missing-exec = Fix: Hãy cài đặt { $exec } và/hoặc nó nằm trong PATH của bạn.
check-env = Môi trường
check-env-editor = Biến EDITOR được đặt?
check-env-editor-exec = EDITOR ({ $exec }) có thể sử dụng?
check-env-editor-vi = Editor vi có thể sử dụng thay thế?
check-env-exec = { $exec } đã cài và có thể sử dụng?
check-env-lang = { $cmd } có chứa giá trị LANG? ({ $lang })
check-env-lang-fix = Cập nhật { $file } để thêm { $lang }.
check-env-lang-fix2 = Hãy đặt biến LANG!
check-env-lang-known = Aura được dịch theo LANG?
check-env-java-bin = Các công cụ Java đã được cài?
check-env-java-bin-fix = Cân nhắc cài { $pkg }.
check-env-java-set = Môi trường Java được đặt?
check-env-java-set-fix = Xem thêm { $cmd }.
check-pconf = Cấu hình Pacman (/etc/pacman.conf)
check-pconf-par = Đã kích hoạt Parallel downloads?
check-pconf-par-fix = { $setting } đã tắt, hoặc đặt thành 1. Đặt { $set } để tải dữ liệu nhanh hơn.
check-pconf-ignores = Không có các gói đã bỏ qua trùng lặp?
check-pconf-ignores-fix = Các gói sau đã bị bỏ qua cả trong pacman.conf và aura.toml: { $pkgs }
check-pconf-pacnew = Các tệp .pacnew có liên quan?
check-pconf-pacnew-broken = Lỗi: Gọi đến { $fd } thất bại.

check-pconf-pacnew-old = { $path } cũ hơn so với .pacnew khoảng { $days ->
[one] 1 ngày.
  *[many] {$days} ngày.
}

check-aconf = Cấu hình Aura
check-aconf-aura-exists = Tập cấu hình của Aura đã tạo?
check-aconf-aura-exists-fix = Fix: Cân nhắc { $cmd }
check-aconf-aura-parse = Có thể đọc tệp cấu hình của Aura?
check-aconf-old-dirs = Không có thư mục cũ của Aura?
check-aconf-old-conf = Không có tệp cấu hình cũ của Aura?
check-mconf = Cấu hình Makepkg ({ $path })
check-mconf-packager = Đã đặt PACKAGER?
check-mconf-packager-fix = Fix: Đặt { $cmd } trong { $path }
check-snapshots = Lưu trữ các gói
check-snapshot-usable = Tất cả các gói có bản lưu trữ tương ứng?
check-snapshot-usable-fix = Fix: Bạn có thể xoá các bản lưu trữ cũ bằng { $command }
check-cache = Cache các Gói Cài đặt
check-cache-exists = Tồn tại các cache đã chỉ định?
check-cache-tarballs = Các gói cài đặt hợp lệ?
check-cache-tarballs-fix = Fix: Bạn có thể xoá gói không hợp lệ bằng { $command }
check-cache-missing = Các gói chính thức có gói cài đặt?
check-cache-missing-fix = Fix: Bạn có thể tải các gói cài đặt bị thiếu bằng{ $command }
check-cache-missing-for = Các gói AUR đã cài có gói cài đặt?
check-cache-missing-for-fix = Fix: Xem các gói bị thiếu bằng { $cmd } và cài lại chúng thủ công.
check-pkgs = Trạng thái các gói
check-pkgs-old = Tất cả các gói cài trực tiếp, các gói không-phụ-thuộc mới nhất?
check-pkgs-old-warn = { $pkg } cập nhật lần cuối { $days } trước.
check-pkgs-empty = Tất cả bản sao các gói đã đủ thông tin?
check-pkgs-empty-fix = Fix: Xoá bỏ thư mục sau.

# Thanks
thanks-you = Cảm ơn bạn đã sử dụng Aura.
thanks-colin = Aura bởi Colin Woodbury, 2012 - 2024
thanks-pacman = Cảm ơn đến với đội Pacman và Arch Linux đã tạo ra nền móng vững chắc.
thanks-everyone = Cảm ơn tất cả những người đóng góp, nhà tài trợ, và người dùng Aura.
thanks-logo = Logo của Aura bởi Cristiano Vitorino.
thanks-translators = Aura được dịch bởi:

# Configuration (conf)
conf-toml-err = Chuyển đổi cấu hình hiện tại thất bại.

# Dependencies (deps)
deps-io = Tạo image các gói phụ thuộc thất bại.

# Runtime Environment
env-missing-editor = EDITOR được đặt không nằm trong PATH.
env-pconf = Không thể lấy dữ liệu từ tệp pacman.conf.

# Pacman Calls
pacman-external = Gọi đến pacman thất bại.
pacman-u = Gọi đến pacman -U thất bại.
pacman-s = Gọi đến pacman -S thất bại.
pacman-misc = Gọi đến pacman trả về exit code khác 0.

# Aura-specific Directories
dir-mkdir = Tạo thư mục thất bại: { $dir }.
dir-home = Không tìm thấy thư mục cấu hình của Aura.
dir-cache = Không tìm thấy thư mục cache của Aura.

# Dependency Resolution
dep-exist = Gói { $pkg } không tồn tại.
dep-exist-par = Gói phụ thuộc { $pkg } của { $par } không tồn tại.
dep-graph = Biểu đồ phụ thuộc bị lỗi vô định.
dep-cycle = Phát hiện vòng phụ thuộc: { $cycle }
dep-multi = Có nhiều lỗi xảy ra trong khi phân tích gói phụ thuộc.

# Git Operations
git-diff = Git diff thất bại: { $file }
git-hash = Dịch git hash sang Rust thất bại.
git-pull = Git pull thất bại: { $dir }
git-clone = Git clone thất bại: { $dir }
git-io = Gọi đến git thất bại.

# Faur Calls
faur-fetch = Gọi đến máy chủ thông tin thất bại: { $pkg }
faur-unknown = Gói vô định: { $pkg }
faur-too-many = Có nhiều dữ liệu trả về từ Faur hơn dự kiến: { $pkg }

# Common Errors
err-alpm = Khởi chạy ALPM thất bại.
err-config-path = Không tìm được đường dẫn tệp cấu hình của Aura.
err-curl = Thao tác CURL thất bại: { $err }
err-file-del = Không thể xoá: { $file }
err-file-open = Không thể mở tham chiếu tệp tới: { $file }
err-file-write = Không thể ghi tệp: { $file }
err-json-decode = Không thể giải mã JSON từ: { $url }
err-json-write = Không thể ghi JSON tới: { $file }
err-mutex = Mutex bị lỗi.
err-pool-create = Không thể khởi tạo kết nối tới ALPM.
err-pool-get = Không tìm thấy tham chiếu tới ALPM trong kết nối.
err-read-dir = Không thể đọc thư mục: { $dir }
err-srcinfo = Không thể đọc .SRCINFO: { $file }
err-sudo = Chạy Aura bằng sudo là không cần thiết.
err-time-conv = Không thể chuyển đổi timestamp.
err-time-format = Lỗi định dạng chuỗi thời gian.
err-user-input = Không nhận được phản hồi người dùng.
err-utf8 = Lỗi chuyển đổi UTF-8.
err-write = Không thể hiển thị ra stdout.

# Common Fields
# common-yes = Yes
# common-no = No
common-name = Tên
common-done = Xong.
common-no-packages = Chưa chỉ định gói.
common-no-work = Không cần làm gì.
common-cancelled = Hành động bị huỷ.
common-replace = Bạn có thể xoá { $old } vì đã có { $new }.

# Misc.
proceed = Tiếp tục?
# proceed-affirmative = y
# proceed-affirmative-alt = Y
# proceed-negative = n
