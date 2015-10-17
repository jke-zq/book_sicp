#lang racket
;这个算法在《算法导论》里面就有，题目本身说的不是很清楚。P544
;有两种方式可以参考：
;------算法导论求该算法使用到的算法
#|
;while(!(u&1)) {    //如果u为偶数则u右移,用k记录移位数
       k++; u >>= 1;
   }

   srand((ll)time(0));
   for(i = 0; i < S; ++i) {    //进行S次测试
       x = rand()%(n-2) + 2;    //在[2, n)中取随机数
       if((x%n) == 0)    continue;

       x = mod_exp(x, u, n);    //先计算(x^u) % n，
       pre = x;
       for(j = 0; j < k; ++j) {    //把移位减掉的量补上，并在这地方加上二次探测
           x = mod_mul(x, x, n);
           if(x == 1 && pre != 1 && pre != n-1)    return false;    //二次探测定理，这里如果x = 1则pre 必须等于 1，或则 n-1否则可以判断不是素数
           pre = x;
       }
;-------更一般的写法， 找到所有／2的地方。
bool witness(__int64 a,__int64 n)
{
   __int64 t,d,x;
   d=1;
   int i=ceil(log(n-1.0)/log(2.0)) - 1;
   for(;i>=0;i--)
   {
       x=d;  d=(d*d)%n;
       if(d==1 && x!=1 && x!=n-1) return true;
       if( ((n-1) & (1<<i)) > 0)
           d=(d*a)%n;
   }
   return d==1? false : true;
}
|#
(define (square n)
  (* n n))
(define (even? n)
  (= (remainder n 2) 0))
(define (nontrivial-exp base n m)
  (cond ((= n 0) 1)
        ((even? n) 
         (remainder (nontrivial-spare-root? (nontrivial-exp base (/ n 2) m) m) m))
        (else (remainder (* base (nontrivial-exp base (- n 1) m)) m))))

(define (nontrivial-spare-root? a m)
  (if (and (not (= a 1)) 
       (not (= a (- m 1))) 
       (= 1 (remainder (square a) m))) 0 (square a)))

;goood style--from http://community.schemewiki.org/?sicp-ex-1.28
(define (miller-rabin-expmod base exp m) 
   (define (squaremod-with-check x) 
     (define (check-nontrivial-sqrt1 x square) 
       (if (and (= square 1) 
                (not (= x 1)) 
                (not (= x (- m 1)))) 
           0 
           square)) 
     (check-nontrivial-sqrt1 x (remainder (square x) m))) 
   (cond ((= exp 0) 1) 
         ((even? exp) (squaremod-with-check 
                       (miller-rabin-expmod base (/ exp 2) m))) 
         (else 
          (remainder (* base (miller-rabin-expmod base (- exp 1) m)) 
                     m))))
(define (miller-rabin-test a n)
       (= 1 (nontrivial-exp a (- n 1) n)))
;result: 1105----336(fermat)/30(miller-rabin)
(define (miller-rabin n)
  (display n)
  (newline)
  (define (miller-rabin-iter a count)
    (cond ((= a n) (display count))
          ((miller-rabin-test a n) (miller-rabin-iter (+ a 1) (+ 1 count)))
          (else (miller-rabin-iter (+ a 1) count))))
  (miller-rabin-iter 1 0))
  