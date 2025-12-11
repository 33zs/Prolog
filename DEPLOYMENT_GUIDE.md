# 部署指南

## 前置要求

- SWI-Prolog 8.0 或更高版本
- 端口 8080 可用 (或选择其他端口)

## 快速启动

### 1. 启动 Web 界面 (推荐)

```bash
swipl
```

然后在 Prolog 提示符下：

```prolog
?- consult('dungeon_escape.pl').
?- consult('game_web.pl').
?- start_web(8080).
```

打开浏览器访问: `http://localhost:8080/`

### 2. 启动命令行界面

```bash
swipl
```

```prolog
?- consult('dungeon_escape.pl').
?- start.
```

### 3. 启动服务器模式 (JSON API)

```prolog
?- consult('dungeon_escape.pl').
?- consult('game_server.pl').
?- game_server:start_server(8080).
```

## 配置选项

### 更改端口

```prolog
?- start_web(3000).  % 使用端口 3000
```

### 设置难度

```prolog
?- consult('game_learning.pl').
?- game_learning:set_difficulty(hard).
?- start.
```

难度选项: `easy`, `normal`, `hard`

### 查看统计

```prolog
?- consult('game_learning.pl').
?- game_learning:show_statistics.
```

### 查看成就

```prolog
?- game_learning:show_achievements.
```

## 重要文件

| 文件 | 用途 |
|------|------|
| `dungeon_escape.pl` | 主游戏引擎 |
| `game_web.pl` | Web界面 (双语) |
| `game_server.pl` | JSON API服务器 |
| `game_learning.pl` | 统计和学习系统 |
| `player.png` | 玩家头像图片 |

## 故障排除

### 图片不显示

确保 `player.png` 文件在游戏目录中，或者重命名现有的 `bfd862ff7ad1c84fe71e106380b9de4d.png` 为 `player.png`:

```bash
cp bfd862ff7ad1c84fe71e106380b9de4d.png player.png
```

### 端口已被占用

```prolog
?- start_web(8081).  % 尝试其他端口
```

### 停止服务器

```prolog
?- stop_web.
```

或

```prolog
?- game_server:stop_server.
```

## 游戏命令

### 移动
- `n.` - 北
- `s.` - 南
- `e.` - 东
- `w.` - 西

### 互动
- `look.` - 查看周围
- `take(物品).` - 拾取物品
- `drop(物品).` - 丢弃物品
- `use(物品).` - 使用物品

### 信息
- `inventory.` 或 `i.` - 查看背包
- `status.` - 游戏状态
- `gold.` - 查看金币
- `shop.` - 在市场购物

### 战斗
- `attack.` - 攻击敌人

### 秘密命令
- `xyzzy.` - ？？？
- `plugh.` - ？？？
- `pray.` - 祈祷神灵
- `dance.` - 跳舞
- `sing.` - 唱歌

## 游戏目标

1. 找到火把避免陷阱
2. 收集武器和护甲
3. 获得金钥匙
4. 拿到金色宝藏
5. 用钥匙解锁出口
6. 到达出口大厅逃脱！

## 安全提示

- ✅ 所有安全漏洞已修复
- ✅ 输入验证已加强
- ✅ 仅允许白名单命令
- 建议：在生产环境中考虑添加速率限制

## 支持

遇到问题？检查 `FIXES_SUMMARY.md` 了解最新修复。
