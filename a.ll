define i64 @function(i64 %_t1){
%x = alloca i64
store i64 %_t1, i64* %x
br label %_t18
_t18:
%_t3 = load i64, i64* %x
%_t5 = icmp sge i64 %_t3, 1
%_t17 = icmp eq i1 %_t5, 0
br i1 %_t17, label %_t7, label %_t16
_t7:
ret i64 1
_t16:
%_t9 = load i64, i64* %x
%_t11 = load i64, i64* %x
%_t13 = sub i64 %_t11, 1
%_t14 = call i64 @function(i64 %_t13)
%_t15 = mul i64 %_t9, %_t14
ret i64 %_t15
}
