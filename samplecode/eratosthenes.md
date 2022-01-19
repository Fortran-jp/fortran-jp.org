## エラトステネスのふるい

### エラトステネスのふるい

エラトステネスのふるいは，素数を求めるアルゴリズムの一つです．

ある数nが与えられた時に，2からnまでの数表を用意し，まず2の倍数を数表から取り除きます．この作業をふるい落としなどとよびます．数字を3, 41, 5...⌊√n⌋と順次変えてその数の倍数を数表から数をふるい落とし，最後までふるい落とされなかった数を素数として扱います．

### mainプログラム

primeという名前のモジュールの中に，generate_primeという関数を作成します．入力は正の整数で，戻り値は整数の配列とします．

```
program main
    use, intrinsic :: iso_fortran_env
    use :: prime
    implicit none

    integer(int32), allocatable :: primes(:)

    primes = generate_prime(100)
    print '(*(i0:,1x))', primes
end program main
```

配列を返すようにすると，配列が大きくなったときに実行速度が落ちるでしょうが，その場合はサブルーチンに変更して，整数を入力，整数型配列を入出力にします．本記事ではこちらの方法は紹介しません．

### 単純な実装

まずは素直に実装します．

```
module prime
    implicit none
    private
    public :: generate_prime
contains
    function generate_prime(num) result(primes)
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32), intent(in) :: num
        integer(int32), allocatable :: primes(:)
        integer(int32), allocatable :: number(:)

        if (num >= 2) then

            allocate (number(2:num))

            initialize: block
                integer(int32) :: i
                do i = 2, num
                    number(i) = i
                end do
            end block initialize

            sieve: block
                integer(int32) :: i, prime_candidate
                do prime_candidate = 2, int(num**0.5)
                    if (number(prime_candidate) /= 0) then
                        do i = prime_candidate + prime_candidate, num, prime_candidate
                            number(i) = 0
                        end do
                    end if
                end do
            end block sieve

            extract: block
                integer(int32) :: i, j, num_nonzero

                num_nonzero = 0
                do i = 2, num
                    if (number(i) /= 0) num_nonzero = num_nonzero + 1
                end do

                allocate (primes(num_nonzero))

                j = 1
                do i = 2, num
                    if (number(i) /= 0) then
                        primes(j) = number(i)
                        j = j + 1
                    end if
                end do
            end block extract
        else
            allocate (primes(0))
        end if
    end function generate_prime
end module prime
```

1は素数ではないので，引数が2未満の場合は，長さ0の配列を割り付けて呼出し元に返します．

引数が2以上の場合に素数を求める処理を行います．このとき，エラトステネスのふるい処理は三つに分けることができます．

### 初期化

2からnumまでの数表numberを作ります．このとき，配列添字と配列に保持される数字を対応させるために，配列添字を2:numとして割り付けています． 

```
allocate (number(2:num))

initialize: block
    integer(int32) :: i
    do i = 2, num
        number(i) = i
    end do
end block initialize
```


### ふるい落とし

数表numberを作った後，2からnumの平方根までを素数の候補として，その倍数を0で置き換えます．これは0である必要はなく，1でも-1でもよいのですが，0以外が用いられている例は見たことがりません． 

```
sieve: block
    integer(int32) :: i, prime_candidate
    do prime_candidate = 2, int(num**0.5)
        if (number(prime_candidate) /= 0) then
            do i = prime_candidate*2, num, prime_candidate
                number(i) = 0
            end do
        end if
    end do
end block sieve
```


### 素数の抽出

倍数を0で埋め終わると，数表numberには0に置き換えられていない数がいくつか残ります．これが求められた素数なので，数表numberからそれらの数を取り出し，素数表primesに代入します．



手続きとしては，まず数表numberの非ゼロの要素数を数え，その数で整数型配列primesを割り付けます．その後，再び数表内の非ゼロ要素を探し，その都度素数表primesにコピーします．

```
extract: block
    integer(int32) :: i, j, num_nonzero

    num_nonzero = 0
    do i = 2, num
        if (number(i) /= 0) num_nonzero = num_nonzero + 1
    end do

    allocate (primes(num_nonzero))

    j = 1
    do i = 2, num
        if (number(i) /= 0) then
            primes(j) = number(i)
            j = j + 1
        end if
    end do
end block extract
```

Fortranでは，関数内で宣言されたallocatableな変数は，関数から抜けると自動で解放されるので，後処理は必要ありません．

プログラムを実行すると，下記のように素数が出力されます．

```
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
```

### Fortranの機能を使った実装

前節ではエラトステネスのふるいを素直に実装しましたが，Fortranの機能を用いると，ソースを簡略化できます． 

```
    function generate_prime(num) result(primes)
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32), intent(in) :: num
        integer(int32), allocatable :: primes(:)
        integer(int32), allocatable :: number(:)
        integer(int32) :: i

        if (num >= 2) then
            allocate (number(2:num), source=[(i, i=2, num)])

            do i = 2, int(num**0.5)
                if (number(i) /= 0) then
                    number(i+i::i) = 0
                end if
            end do

            primes = pack(array=number, mask=(number /= 0))
        else
            allocate (primes(0))
        end if
    end function generate_prime
```


どのように簡略化したかを，初期化，ふるい落とし，素数の抽出に分けて見てみます．

### 初期化

```
allocate (number(2:num), source=[(i, i=2, num)])
```

配列構成子[(i, i=2, num)]で2,3,...,num-1,numを要素に持つ配列リテラルを作り，それをsourceに指定することで，numberのメモリ割付時に2,3,...,num-1,numで初期化されるようにしました．

### ふるい落とし

```
do i = 2, int(num**0.5)
    if (number(i) /= 0) then
        number(i+i::i) = 0
    end if
end do
```

大まかな処理は変わりませんが，配列の下限，上限，増分を指定することで，素数候補iの倍数を0にするループを消しました．上限が未指定の場合は，配列の上限値が採用されます．

### 素数の抽出

```
primes = pack(array=number, mask=(number /= 0))
```

arrayに指定した配列のうち，maskが真になる位置の要素だけを抽出するpack関数を用いることで，素数表primesの割付，数表number内の非ゼロ要素の抽出を1行で行えるようにしました． 

### 論理型を用いる実装

数表を用いる実装では，ある数の倍数を0にすることで，0以外の数が素数であると判断しました．先述していますが，この0は0である必要はなく，任意性が存在します．

配列操作がFortranほど得意でない他言語では，論理値（あるいは0,1）を用いた実装もよく行われます．つまり，素数であれば真（または1），素数でなければ偽（または0）とする実装です．

Fortranでは，論理値を用いる実装も簡単です

```
    function generate_prime(num) result(primes)
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32), intent(in) :: num
        integer(int32), allocatable :: primes(:)
        logical, allocatable :: is_prime(:)
        integer(int32) :: i

        if (num >= 2) then
            allocate (is_prime(2:num), source=.true.)

            do i = 2, int(num**0.5)
                if (is_prime(i)) then
                    is_prime(i+i::i) = .false.
                end if
            end do

            primes = pack(array=[(i, i=2, num)], mask=is_prime)
        else
            allocate (primes(0))
        end if
    end function generate_prime
```

| 変更箇所	 | 変更前 | 変更後 |
----|----|----|
| 変数宣言	 | integer(int32), allocatable :: number(:)	 | logical, allocatable :: is_prime(:) |
| 初期化	 | allocate (number(2:num), source=[(i, i=2, num)])	 | allocate (is_prime(2:num), source=.true.) |
| ふるい落とし	 | if (number(i) /= 0)	 | if (is_prime(i)) |
| ふるい落とし	 | number(i+i::i) = 0	 | is_prime(i+i::i) = .false. |
| 素数の抽出	 | primes = pack(array=number, mask=(number /= 0))	 | primes = pack(array=[(i, i=2, num)], mask=is_prime) |

数表numberを用いていましたが，is_primeと変更する事で，iは素数か？（is_prime(i)）を真偽で表すように変更しました．

初期化の際に，とりあえず全ての数を素数とするようにしました．

ふるい落としの処理は，iが素数であれば(if (is_prime(i)))，その倍数は素数でない(is_prime(i+i::i) = .false.)としました．

素数の抽出処理は，まず配列構成子によって2からnumまでの配列リテラルを作成し，その要素のうち，is_prime(i)が真の要素だけを抽出するようにしました．

### リファクタリング

さて，前節までのプログラムは，エラトステネスのふるいを実装するという意味では，特に問題はないと思います．

しかし，アジャイルソフトウェア開発の奥義などリファクタリングに関する本を読んでいると，これは適切なプログラムではないと思えてきます．適切でない点を挙げてみると

1. generate_prime(num)が，numまでの素数を作るのか，num個の素数を作るのかが明確でない．

1. 素数を求める処理が，初期化，ふるい落とし，素数の抽出に分かれているが，ソースからはそれが明確でない．

1. num>=2やint(num**0.5)の意図が明確でない．

1. エラトステネスのふるいのアルゴリズムが行っていることを正確に反映していない．

アジャイルソフトウェア開発の奥義3で示されている考え方を参考に，まずは，1., 2., 3.についてリファクタリングを試みます．

1.については前者を想定しているので，関数名をgenerate_primeから，generate_primes_up_toに変更し，引数で渡された数までの素数を作る事を明確にします．

2.については，モジュール手続きinitialize, sieve, extract_primesを導入します．

3.についても，内部関数を導入することで，一読した際に何をしているかが明確になるようにします．

```
module prime
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: generate_primes_up_to

    logical, private, allocatable :: is_prime(:)
    integer(int32), private, parameter :: min_val = 2
    integer(int64), private, parameter :: max_val = huge(0_int32)
    integer(int32), private :: num

contains
    function generate_primes_up_to(number) result(primes)
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32), intent(in) :: number
        integer(int32), allocatable :: primes(:)

        if (in_range()) then
            call initialize(number)

            call sieve()

            primes = extract_primes()

            deallocate (is_prime)
        else
            allocate (primes(0))
        end if
    contains
        logical function in_range()
            use, intrinsic :: iso_fortran_env
            implicit none

            if (min_val <= number .and. number <= max_val) then
                in_range = .true.
            else
                in_range = .false.
            end if
        end function in_range
    end function generate_primes_up_to

    subroutine initialize(number)
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32), intent(in) :: number

        num = number
        allocate (is_prime(min_val:num), source=.true.)
    end subroutine initialize

    subroutine sieve()
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32) :: i

        do i = min_val, determine_iteration_limit()
            if (is_prime(i)) then
                is_prime(i+i:num:i) = .false.
            end if
        end do
    contains
        function determine_iteration_limit() result(iteration_limit)
            use, intrinsic :: iso_fortran_env
            implicit none
            integer(int32) :: iteration_limit

            ! エラトステネスのふるいでは，nまでの素数�'求める場合，√nまで繰り�"せばよい�"とが証��されている．
            iteration_limit = int(num**0.5)
        end function determine_iteration_limit
    end subroutine sieve

    function extract_primes() result(primes)
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32), allocatable :: primes(:)

        integer(int32) :: i

        primes = pack(array=[(i, i=min_val, num)], mask=is_prime)
    end function extract_primes
end module prime
```

関数名を変更することで，100までの素数を作る事が明確になりました． 

```
primes = generate_primes_up_to(100)
```

手続きを導入することで，素数を求める処理が，初期化，ふるい落とし，素数の抽出の三つからなることがわかりやすくなりました．

```
call initialize(number)

call sieve()

primes = extract_primes()
```

内部関数を導入することで，一読した際に何らかの範囲内の場合(if (in_range()))に素数を求めることや，ふるい落としの際に繰り返しの上限を定めている(do i = min_val, determine_iteration_limit())ことが明確になりました．

### 更なるリファクタリング

4.を放置していましたので，これを考えます．

エラトステネスのふるいは，ある数numが与えられた際，2からnumまでの数表を用意し，2の倍数を数表からふるい落とします．数字を3, 4, 5...⌊√n⌋と順次変えてその数の倍数を数表から数をふるい落とし，最後までふるい落とされなかった数を素数として扱うのでした．

このとき，我々がふるい落としをすることを考えると，ふるい落とす際に数表に✕を付けていくことでしょう．例えば，10までの素数を求めることを想定すると，数表2, 3, 4, 5, 6, 7, 8, 9, 10を用意し，2の倍数に✕を付け（2, 3, x, 5, x, 7, x, 9, x），次に3の倍数に✕を付け（2, 3, x, 5, x, 7, x, x, x）て，完了とします．

この行為は，最初に全てを素数とし，倍数は素数でないとする処理とは明らかに異なっています．この行為を正確に再現するには，ふるい落とす＝数表におけるある数の倍数に✕を付けること，初期化＝数表に✕が付いていないことを表現する必要があります．英語では，✕を付けることをcross out，✕を取り除くことをuncrossと言うようなので，処理の名前を変更します．つまり，is_primesではなくcrossed_out，initializeではなくuncross_number_list，sieveではなくcross_out_multiples，extract_primesではなくget_uncrossed_integersとすることが望ましいということです．

名前の変更伴って，if文の判定が変わる箇所があります．ふるい落としや素数を抽出する処理で素数かを判定していたところ（if(is_prime(i))およびmask=is_prime）が，✕が付いているかを判定するように変わります（if(crossed_out(i))およびmask=crossed_out）．数表に✕が付いている数は素数ではないので，.not.を付けて真偽を逆にする必要があります．

また，モジュール変数numは，数表における最大値を意味しているので，maximun_value_of_number_listに変更します．

```
module prime
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: generate_primes_up_to

    logical, private, allocatable :: crossed_out(:)
    integer(int32), private, parameter :: min_val = 2
    integer(int64), private, parameter :: max_val = huge(0_int32)
    integer(int32), private :: maximun_value_of_number_list

contains
    function generate_primes_up_to(number) result(primes)
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32), intent(in) :: number
        integer(int32), allocatable :: primes(:)

        if (in_range()) then

            call uncross_number_list(number)

            call cross_out_multiples()

            primes = get_uncrossed_integers()

            deallocate (crossed_out)
        else
            allocate (primes(0))
        end if
    contains
        logical function in_range()
            use, intrinsic :: iso_fortran_env
            implicit none

            if (min_val <= number .and. number <= max_val) then
                in_range = .true.
            else
                in_range = .false.
            end if
        end function in_range
    end function generate_primes_up_to

    subroutine uncross_number_list(number)
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32), intent(in) :: number

        maximun_value_of_number_list = number
        allocate (crossed_out(min_val:maximun_value_of_number_list), source=.false.)
    end subroutine uncross_number_list

    subroutine cross_out_multiples()
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32) :: i

        do i = min_val, determine_iteration_limit()
            if (.not. crossed_out(i)) then
                call cross_out_multiples_of(i)
            end if
        end do
    contains
        function determine_iteration_limit() result(iteration_limit)
            use, intrinsic :: iso_fortran_env
            implicit none
            integer(int32) :: iteration_limit

            ! エラトステネスのふるいでは，nまでの素数�'求める場合，繰り�"�-は√nまででよい�"とが証��されている．
            iteration_limit = int(maximun_value_of_number_list**0.5)
        end function determine_iteration_limit

        subroutine cross_out_multiples_of(num)
            use, intrinsic :: iso_fortran_env
            implicit none
            integer(int32), intent(in) :: num
            crossed_out(num+num::num) = .true.
        end subroutine cross_out_multiples_of
    end subroutine cross_out_multiples

    function get_uncrossed_integers() result(primes)
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32), allocatable :: primes(:)

        integer(int32) :: i

        primes = pack(array=[(i, i=min_val, maximun_value_of_number_list)], mask=(.not. crossed_out))
    end function get_uncrossed_integers
end module prime
```

同様に変更箇所を表にします．

| 変更箇所	 | 変更前 | 変更後 |
----|----|----|
| 変数宣言	 | logical, allocatable :: is_prime(:) | logical, private, allocatable :: crossed_out(:) |
| 初期化	 | allocate (is_prime(min_val:num), source=.true.)	 | allocate (crossed_out(min_val:maximun_value_of_number_list), source=.false.) |
| ふるい落とし	 | if (is_prime(i))		 | if (.not. crossed_out(i)) |
| ふるい落とし	 | is_prime(i + i::i) = .false.		 | crossed_out(num+num::num) = .true. |
| 素数の抽出	 | primes = pack(array=[(i, i=min_val, num)], mask=is_prime)	 | pack(array=[(i, i=min_val, maximun_value_of_number_list)], mask=(.not. crossed_out)) |

iは素数か？ではなく，iは数表で✕が付けられているか？（いずれかの数の倍数か？）と判定するようになりました．

初期化の際に，全ての数を素数とするのではなく，数表で✕が付けられていないと扱うようになりました．

ふるい落としの処理は，数表でiに✕が付いていなければ(if (.not. crossed_out(i)))，その倍数に✕を付ける(crossed_out(num+num::num) = .true.)としました．また，内部手続きcall cross_out_multiples_of(i)を導入して，一読して何をしているかをわかりやすくしました．

素数の抽出処理は，配列構成子によって2からmaximun_value_of_number_listまでの配列リテラルを作成し，その要素のうち，crossed_out(i)が偽の要素だけを抽出するようにしました．すなわち，数表に✕が付いていない＝何らかの数の倍数ではない＝素数と判断しているということです．

### pack関数の置き換え

pack関数がわかりにくいという場合は，再自動割付配列を利用して書き直すことができます．

```
        ! primes = pack(array=[(i, i=min_val, maximun_value_of_number_list)], mask=(.not. crossed_out))
        primes = [integer ::] ! 整数型，長さ0の配�-�'割り��'る
        do i = min_val, maximun_value_of_number_list
            if (.not. crossed_out(i)) then
                primes = [primes, i] ! 現在のリストの末尾にi�'追加する
            end if
        end do
```

### 条件式の変更

条件式には否定形よりも肯定形を用いるのが良いとされています．そのため，if(.not. crossed_out(i))とならないように，モジュール関数uncrossedを作成して，条件式が肯定形になるようにします．

```
    logical function uncrossed(list_position)
        use, intrinsic :: iso_fortran_env
        implicit none

        integer(int32), intent(in) :: list_position
        if (.not. crossed_out(list_position)) then
            uncrossed = .true.
        else
            uncrossed = .false.
        end if
    end function uncrossed
```

モジュール中に出てくるif(.not. crossed_out(i))をif(uncrossed(i))に置き換えれば完成です．

```
module prime
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: generate_primes_up_to

    logical, private, allocatable :: crossed_out(:)
    integer(int32), private, parameter :: min_val = 2
    integer(int64), private, parameter :: max_val = huge(0_int32)
    integer(int32), private :: maximun_value_of_number_list

contains
    function generate_primes_up_to(number) result(primes)
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32), intent(in) :: number
        integer(int32), allocatable :: primes(:)

        if (in_range()) then

            call uncross_number_list(number)

            call cross_out_multiples()

            primes = get_uncrossed_integers()

            deallocate (crossed_out)
        else
            allocate (primes(0))
        end if
    contains
        logical function in_range()
            use, intrinsic :: iso_fortran_env
            implicit none

            if (min_val <= number .and. number <= max_val) then
                in_range = .true.
            else
                in_range = .false.
            end if
        end function in_range
    end function generate_primes_up_to

    subroutine uncross_number_list(number)
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32), intent(in) :: number

        maximun_value_of_number_list = number
        allocate (crossed_out(min_val:maximun_value_of_number_list), source=.false.)
    end subroutine uncross_number_list

    subroutine cross_out_multiples()
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32) :: i

        do i = min_val, determine_iteration_limit()
            if (uncrossed(i)) then
                call cross_out_multiples_of(i)
            end if
        end do
    contains
        function determine_iteration_limit() result(iteration_limit)
            use, intrinsic :: iso_fortran_env
            implicit none
            integer(int32) :: iteration_limit

            ! エラトステネスのふるいでは，nまでの素数�'求める場合，繰り�"�-は√nまででよい�"とが証��されている．
            iteration_limit = int(maximun_value_of_number_list**0.5)
        end function determine_iteration_limit

        subroutine cross_out_multiples_of(num)
            use, intrinsic :: iso_fortran_env
            implicit none
            integer(int32), intent(in) :: num
            crossed_out(num+num::num) = .true.
        end subroutine cross_out_multiples_of
    end subroutine cross_out_multiples

    function get_uncrossed_integers() result(primes)
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32), allocatable :: primes(:)

        integer(int32) :: i

        primes = [integer ::]
        do i = min_val, maximun_value_of_number_list
            if (uncrossed(i)) then
                primes = [primes, i]
            end if
        end do
    end function get_uncrossed_integers

    logical function uncrossed(list_position)
        use, intrinsic :: iso_fortran_env
        implicit none

        integer(int32), intent(in) :: list_position
        if (.not. crossed_out(list_position)) then
            uncrossed = .true.
        else
            uncrossed = .false.
        end if
    end function uncrossed
end module prime
```

### まとめ

エラトステネスのふるいをFortranで実装し，アジャイルソフトウェア開発の奥義[3]で示されている考え方に沿ってリファクタリングを行いました．

これをアジャイルというかは判りませんが，この程度の書き方であれば，Fortranでも十分に可能です．一方で，Fortranが対象とする問題は，大規模かつ計算速度が重視される場合がほとんどなので，このような書き方が適切であるかは議論が必要です．

最近のFortranは肥大化して無駄という意見もあるでしょうが，このような書き方ができる事実をどのように保守性の向上に繋げるか，メモリ使用量の低減や高速な計算とどうやって両立させるか等も含め，議論を深める必要があると考えています．



1. 4の倍数は，既に2の倍数としてふるい落とされている． 

1. 未だにFortranなのですか？XXに乗りv換えないのですか？と尋ねられる事もありますが，そのXXに入る言語でもこのような実装をしている（あるいは言語固有の機能でもっと判りにくく書いてある）ので，問題はないのでしょう． 

1. Martin, C.R., 瀬谷訳, アジャイルソフトウェア開発の奥義 第2版, SBクリエイティブ, 東京 (2008). https://www.sbcr.jp/product/4797347784/ 

