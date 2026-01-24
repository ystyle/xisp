# Lisp核心功能设计（不依赖仓颉桥接）

## 一、**绝对必要的最小核心**

### 1.1 **基础数据结构**
```lisp
;; 原子类型
42           ; 整数
3.14         ; 浮点数
"hello"      ; 字符串
true / false ; 布尔值
nil          ; 空值
:symbol      ; 符号（关键字）
'foo         ; 符号（引用）

;; 组合类型
'(1 2 3)     ; 链表（cons cells）
()           ; 空表
```

### 1.2 **核心特殊形式（Special Forms）**
这些**必须**在解释器中实现，无法用函数替代：

```lisp
;; 1. 定义变量
(define x 10)
(set! x 20)     ; 修改已有变量

;; 2. 定义函数
(define (add a b) (+ a b))
(lambda (x) (* x x))    ; 匿名函数

;; 3. 条件判断
(if condition then-expr else-expr)
(condb
  test1 expr1
  test2 expr2
  else default)

;; 4. 引用与求值
(quote x)   ; 或 'x - 不求值
(eval expr) ; 对表达式求值

;; 5. 顺序执行
(begin
  expr1
  expr2
  expr3)

;; 6. 局部绑定
(let ((x 1) (y 2))
  (+ x y))

;; 7. 函数应用
(apply func arg-list)
```

### 1.3 **基础函数库（可纯Lisp实现）**

#### **算术运算**
```lisp
(+ 1 2 3)       ; 6
(- 10 5)       ; 5
(* 2 3 4)      ; 24
(/ 10 2)       ; 5
(mod 10 3)     ; 1
(= 1 1)        ; true
(< 3 5)        ; true
(> 10 5)       ; true
```

#### **列表操作**
```lisp
;; 构造与分解
(cons 1 '(2 3))        ; (1 2 3)
(car '(1 2 3))         ; 1
(cdr '(1 2 3))         ; (2 3)
(list 1 2 3)           ; (1 2 3)

;; 查询
(length '(a b c))      ; 3
(null? '())            ; true
(member 2 '(1 2 3))    ; (2 3)

;; 高阶操作
(map (lambda (x) (* x x)) '(1 2 3))   ; (1 4 9)
(filter even? '(1 2 3 4))             ; (2 4)
(reduce + 0 '(1 2 3))                 ; 6
```

#### **逻辑运算**
```lisp
(and true false)      ; false
(or true false)       ; true
(not true)            ; false
```

#### **谓词函数**
```lisp
(number? 42)          ; true
(string? "hello")     ; true
(symbol? 'foo)        ; true
(list? '(1 2))        ; true
(procedure? +)        ; true
```

### 1.4 **环境与作用域**
```lisp
;; 环境操作
(current-environment)   ; 获取当前环境
(define-environment)    ; 创建新环境
(set-environment! env)  ; 切换环境

;; 闭包支持（自动实现）
(define (make-adder n)
  (lambda (x) (+ x n)))  ; 闭包捕获n
```

## 二、**解释器核心架构**

### 2.1 **最小可工作解释器**
```cangjie
// 核心数据结构
enum LispValue {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Symbol(String),
    Cons(Box<ConsCell>),
    Procedure(Procedure),
}

struct ConsCell {
    car: LispValue,
    cdr: LispValue,
}

struct Procedure {
    params: Vec<String>,  // 形参列表
    body: LispValue,      // 函数体
    env: Environment,     // 捕获的环境
}

// 求值器核心
impl Evaluator {
    fn eval(&mut self, expr: LispValue, env: &Environment) -> Result<LispValue> {
        match expr {
            // 原子类型直接返回
            LispValue::Number(n) => Ok(LispValue::Number(n)),
            LispValue::String(s) => Ok(LispValue::String(s)),
            
            // 符号 -> 环境查找
            LispValue::Symbol(name) => env.lookup(&name),
            
            // 列表 -> 函数调用或特殊形式
            LispValue::Cons(cell) => {
                let first = self.eval(cell.car, env)?;
                match first {
                    // 特殊形式
                    LispValue::SpecialForm(form) => self.eval_special(form, cell.cdr, env),
                    // 函数调用
                    LispValue::Procedure(proc) => self.apply_procedure(proc, cell.cdr, env),
                    _ => Err("Not callable".into()),
                }
            }
            
            // 其他...
        }
    }
    
    fn eval_special(&mut self, form: SpecialForm, args: LispValue, env: &Environment) {
        match form {
            SpecialForm::Define => self.eval_define(args, env),
            SpecialForm::Lambda => self.eval_lambda(args, env),
            SpecialForm::If => self.eval_if(args, env),
            SpecialForm::Quote => self.eval_quote(args),
            // ...
        }
    }
}
```

### 2.2 **必需的特殊形式实现**

```cangjie
// 实现define
fn eval_define(&mut self, args: LispValue, env: &mut Environment) {
    match args {
        // (define x 10)
        Cons(cell) if cell.cdr is Symbol => {
            let name = cell.car.as_symbol();
            let value = self.eval(cell.cdr.car, env)?;
            env.define(name, value);
        }
        // (define (add x y) (+ x y))
        Cons(cell) if cell.car is Cons => {
            let func_def = cell.car.as_cons();
            let name = func_def.car.as_symbol();
            let params = self.parse_params(func_def.cdr);
            let body = cell.cdr;
            
            let proc = Procedure::new(params, body, env.clone());
            env.define(name, LispValue::Procedure(proc));
        }
    }
}

// 实现lambda
fn eval_lambda(&mut self, args: LispValue, env: &Environment) {
    let params = self.eval(args.car, env)?;  // 参数列表
    let body = args.cdr;                     // 函数体
    
    LispValue::Procedure(Procedure {
        params: self.parse_params(params),
        body,
        env: env.clone(),  // 捕获当前环境形成闭包
    })
}

// 实现if
fn eval_if(&mut self, args: LispValue, env: &Environment) {
    let condition = self.eval(args.car, env)?;
    if is_truthy(condition) {
        self.eval(args.cdr.car, env)  // then分支
    } else {
        self.eval(args.cdr.cdr.car, env)  // else分支
    }
}
```

## 三、**可选但推荐的核心扩展**

### 3.1 **错误处理**
```lisp
;; 错误类型
(error "Something went wrong")
(raise "Error message")

;; 错误处理（可以基于lambda实现）
(try
  (dangerous-operation)
  (catch (lambda (err) (println "Error:" err))))

;; 断言
(assert (= 2 (+ 1 1)))
```

### 3.2 **宏系统（编译时元编程）**
```lisp
;; 定义宏
(defmacro (when condition . body)
  `(if ,condition
       (begin ,@body)
       nil))

;; 使用宏
(when (> x 10)
  (println "x is big")
  (set! x 10))

;; 展开宏
(macroexpand '(when test (do-something)))
```

### 3.3 **尾递归优化**
```lisp
;; 自动优化尾递归，不消耗栈空间
(define (sum n acc)
  (if (= n 0)
      acc
      (sum (- n 1) (+ acc n))))  ; 尾调用，自动优化
```

### 3.4 **简单I/O（纯内存）**
```lisp
;; 仅字符串操作，不依赖系统
(read-string "input")  ; 从字符串读取S表达式
(write-to-string expr) ; 将表达式转为字符串

;; 标准输入输出（简单实现）
(print "Hello")
(read-line)  ; 从预定义输入流读取
```

## 四、**MVP实现路线图（仅核心）**

### **阶段1：骨架（1周）**
- [ ] `LispValue` 枚举定义
- [ ] 递归下降Parser
- [ ] 基础环境（Environment）实现
- [ ] `eval` 函数骨架

### **阶段2：特殊形式（1周）**
- [ ] `quote` / `eval`
- [ ] `define` / `set!`
- [ ] `lambda` / 闭包
- [ ] `if` / `cond`

### **阶段3：基础函数（1周）**
- [ ] 算术：`+ - * / = < >`
- [ ] 列表：`cons car cdr list`
- [ ] 逻辑：`and or not`
- [ ] 谓词：`number? string? null?`

### **阶段4：实用功能（1周）**
- [ ] `begin` 顺序执行
- [ ] `let` 局部绑定
- [ ] `map` / `filter` / `reduce`
- [ ] 错误处理基础

## 五、**测试用例（验证核心功能）**

```lisp
;; 测试1：基础求值
(eval '(+ 1 2 3))  ; => 6

;; 测试2：变量和函数
(define x 10)
(define (add a b) (+ a b))
(add x 5)  ; => 15

;; 测试3：闭包
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(let ((c (make-counter)))
  (c)  ; => 1
  (c)) ; => 2

;; 测试4：高阶函数
(map (lambda (x) (* x x)) '(1 2 3 4))  ; => (1 4 9 16)

;; 测试5：递归
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5)  ; => 120
```

## 六、**设计原则（仅核心）**

1. **自包含**：不依赖任何外部库即可运行
2. **最小化**：核心解释器 < 2000行代码
3. **可扩展**：通过闭包和宏支持高级特性
4. **正确性**：通过S表达式自举测试

## **关键结论**：

一个完整的Lisp核心可以在**4周内实现**，包含：
- ✅ 完整的S表达式解析
- ✅ 词法作用域 + 闭包
- ✅ 特殊形式：`define` `lambda` `if` `quote`
- ✅ 基础数据类型：数字、字符串、符号、列表
- ✅ 内置函数：算术、列表操作、逻辑运算

**这个核心已经是一个图灵完备的语言**，可以编写任何算法。所有IO、系统交互等都可以作为**可选的桥接层**实现，而不是核心的一部分。
