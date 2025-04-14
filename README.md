/**************************** CodeGeeX Inline Diff ****************************/
# 二进制文件I/O模块文档
# Binary File I/O Module Documentation
一个用于处理二进制文件输入/输出操作的Fortran模块，支持多维数组。

## 特点
A Fortran module for handling binary file input/output operations supporting multi-dimensional arrays.
- 支持1D到7D数组操作
- 直接访问二进制文件处理
- 支持多种数据类型：
    - 单精度实数 (real(4))
    - 双精度实数 (real(8))
    - 单精度复数 (complex(4))
    - 双精度复数 (complex(8))

## 使用方法
## Features
### 初始化

- Supports 1D to 7D array operations
- Direct access binary file handling
- Support for multiple data types:
    - Single precision real (real(4))
    - Double precision real (real(8))
    - Single precision complex (complex(4))
    - Double precision complex (complex(8))

## Usage

### Initialization

```fortran
type(BinaryFile) :: bf
call bf%initialize(directory, file_tags, process_id, sample_data)
```
参数：
- `directory`: 目标目录路径

Parameters:
- `directory`: Target directory path
- `file_tags`: Array of 3 string tags for filename construction
- `process_id`: Process identifier
- `sample_data`: Sample array for determining record size

### Write Operations

```fortran
call bf%write(record_number, array_data)
```

Parameters:
- `record_number`: Record number to write to
- `array_data`: Array data to write (1D-7D supported)

### Read Operations

```fortran
call bf%read(record_number, array_data)
```

Parameters:
- `record_number`: Record number to read from
- `array_data`: Array to store read data (1D-7D supported)

### Close File

```fortran
call bf%close()
```

## Error Handling

The module includes basic error checking for:
- File opening failures
- Attempts to read/write to unopened files
- Unsupported data types


/******************** 8e35eccc-4acc-4290-bdcf-da233d30690e ********************/