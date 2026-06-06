
# defprotocol-from-interface

从 Java 类或接口自动生成 Clojure 协议及实现。

无需再手写冗长的 `defprotocol` 和 `extend-type` 样板代码，**defprotocol-from-interface** 允许你通过声明式配置自动完成这些工作，同时支持重命名、过滤、危险标记、委托穿透、自定义实现和包装器等功能。

## 安装

在 `deps.edn` 中添加依赖：

```clojure
{:deps {top.kzre/use-class {:git/url "https://github.com/huixiangchen0314/use-class.git"
                                             :sha "..."}}}
```

或通过 Maven / Leiningen（需要本地安装 `lein install`）。

## 快速开始

```clojure
(require '[top.kzre.use-class.core :refer [use-class]])

;; 从 java.util.Date 生成协议并自动实现 getTime 方法
(use-class java.util.Date
  :protocol-name 'IDate
  :only ['getTime])

;; 现在可以对 Date 实例使用协议方法
(get-time (java.util.Date. 12345)) ;; => 12345
```

## 核心宏

### `defprotocol-from-type`

从 Java 类生成 Clojure 协议，不提供实现。

```clojure
(require '[top.kzre.use-class.core :refer [defprotocol-from-type]])

(defprotocol-from-type java.util.Date
  :protocol-name 'IDateReadOnly
  :only ['getTime 'getMonth])
```

展开为：

```clojure
(defprotocol IDateReadOnly
  (getTime [this])
  (getMonth [this]))
```

### `use-class`

同时生成协议和 `extend-type` 实现，这是最常用的宏。

```clojure
(use-class java.util.Date
  :protocol-name 'IDate
  :rename {'getTime 'get-time}
  :only ['get-time]
  :dangerous #{'set-time}
  :wrappers {:global ['my-logger]})
```

展开为：

```clojure
(do
  (defprotocol IDate
    (get-time [this]))
  (extend-type java.util.Date
    IDate
    (get-time [this] (my-logger (fn [this] (. this getTime))))))
```

## 配置选项

| 选项 | 说明 | 默认值 |
|------|------|--------|
| `:protocol-name` | 协议名称（符号）。若不提供则自动推导为 `I` + 类名。 | `(derive-protocol-name ...)` |
| `:rename` | 方法名重命名映射，`{原始名 → 新名}`。 | `{}` |
| `:rename-fn` | 重命名函数，用于未在 `:rename` 中指定的方法。 | `nil` |
| `:prefix` | 为所有方法名添加前缀。 | `nil` |
| `:only` | 只保留这些方法（符号序列）。 | `nil`（所有方法） |
| `:except` | 排除这些方法（符号序列）。 | `nil` |
| `:dangerous` | 显式危险方法集合，这些方法会被自动添加 `!` 后缀。 | `#{}` |
| `:setter-danger?` | 是否自动识别 setter 并添加 `!`（默认开启）。 | `true` |
| `:delegate` | 委托配置（见下文）。 | `[]` |
| `:custom` | 自定义实现配置 `[方法名 参数个数 实现函数]`。 | `[]` |
| `:wrappers` | 包装器配置 `{:global [...] :methods {方法名 [...]}}`。 | `{}` |
| `:delegate-classes` | 限制智能委托查找的目标类集合。 | `nil`（不限制） |

## 委托支持

通过 `:delegate` 可以声明那些不属于宿主类，但可以通过 getter 获得委托对象再调用的方法。

### 单元素（自动委托）

```clojure
;; 将 Calendar.getTime() 返回的 Date 的所有方法全部委托到协议中
(use-class java.util.Calendar
  :delegate [['getTime]])   ;; 单引号内是 getter 名称
```

### 两元素（同名委托）

```clojure
;; 协议方法 get-month，通过 getTime() 获取 Date，再调用 Date.getMonth()
(use-class java.util.Calendar
  :delegate [['get-month 'getTime]])
```

### 三元素（显式指定目标方法）

```clojure
;; 协议方法 my-get-time，getter 为 getTime()，目标方法为 Date.getTime()
(use-class java.util.Calendar
  :delegate [['my-get-time 'getTime 'getTime]])
```

### 手动映射

```clojure
;; 只委托 getMonth 和 getYear 并自定义方法名
(use-class java.util.Calendar
  :delegate [['getTime [['cal-month 'getMonth] ['cal-year 'getYear]]]])
```

## 自定义实现

用纯 Clojure 函数实现某个协议方法：

```clojure
(defn epoch-seconds [^java.util.Date this]
  (quot (.getTime this) 1000))

(use-class java.util.Date
  :custom [['epoch-seconds 1 'user/epoch-seconds]])
```

## 包装器

包装器是高阶函数，可用于日志、缓存、权限控制等。执行顺序：**列表中最后的包装器在最外层**。

```clojure
(defn log-call [f]
  (fn [this & args]
    (println "Calling" args)
    (apply f this args)))

(use-class java.util.Date
  :wrappers {:global ['user/log-call]
             :methods {'getTime ['user/validate]}})
```

## 前缀

```clojure
(use-class java.util.Date
  :prefix "date-"
  :only ['getTime 'setTime])
;; 生成 date-get-time 和 date-set-time! 方法
```

## 命名冲突处理

如果协议方法名与 `clojure.core` 中的函数重名（如 `get`、`set`），库会自动过滤并发出警告。用户也可通过 `:rename` 或 `:prefix` 手动解决。

## 最佳实践

- **优先使用 `:only` 和 `:except`** 精确控制协议包含的方法，避免污染命名空间。
- **显式委托推荐三元素形式**，可读性最好。
- **对于复杂的映射（如参数类型转换），保留手写 `extend-type`**，本库用于覆盖大多数简单委托场景。
- **在顶层使用宏**，确保协议在编译期定义。

## 许可证

MIT License
