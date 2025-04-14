# 二进制文件 I/O 模块文档

一个用于处理多维度数组二进制文件输入/输出的 Fortran 模块。

## 特性

- 支持 1D 到 7D 数组操作
- 直接访问二进制文件处理
- 支持多种数据类型:
    - 单精度实数 (real(4))
    - 双精度实数 (real(8))
    - 单精度复数 (complex(4))
    - 双精度复数 (complex(8))

## 使用方法

### 初始化

type(BinaryFile) :: bf
call bf%initialize(directory, file_tags, process_id, sample_data)

参数:
- `directory`: 目标目录路径
- `file_tags`: 用于文件名构造的 3 个字符串标签数组
- `process_id`: 进程标识符
- `sample_data`: 示例数组，用于确定记录大小

### 写操作

call bf%write(record_number, array_data)

参数:
- `record_number`: 要写入的记录号
- `array_data`: 要写入的数组数据 (支持 1D-7D)

### 读操作

call bf%read(record_number, array_data)

参数:
- `record_number`: 要读取的记录号
- `array_data`: 存储读取数据的数组 (支持 1D-7D)

### 关闭文件

call bf%close()

## 错误处理

该模块包括基本的错误检查:
- 文件打开失败
- 尝试对未打开的文件进行读/写操作
- 不支持的数据类型

