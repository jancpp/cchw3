define i64 @function(i64 %_t1){
%a = alloca i64
store i64 %_t1, i64* %a
br label %_t18
_t18:
%_t7 = icmp eq i64 1, 1
%_t17 = icmp eq i1 %_t7, 0
br i1 %_t17, label %_t4, label %_t16
_t16:
%_t11 = load i64, i64* %a
%_t13 = icmp eq i64 %_t11, 0
%_t15 = icmp eq i1 %_t13, 0
br i1 %_t15, label %_t9, label %_t14
_t14:
br label %_t4
_t9:
store i64 0, i64* %a
br label %_t18
_t4:
%_t3 = load i64, i64* %a
ret i64 %_t3
}
