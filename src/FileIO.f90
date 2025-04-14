module binary_file_io
  implicit none
  private

  type, public :: BinaryFile
    private
    integer :: file_unit
    integer :: record_bytes
    logical :: is_open = .false.
    character(len=256) :: file_path
  contains
    ! 初始化过程
    procedure :: initialize1d 
    procedure :: initialize2d
    procedure :: initialize3d
    procedure :: initialize4d
    procedure :: initialize5d
    procedure :: initialize6d
    procedure :: initialize7d
    generic   :: initialize => initialize1d, initialize2d, initialize3d, initialize4d, &
                              initialize5d, initialize6d, initialize7d

    ! 写入过程
    procedure :: write1d
    procedure :: write2d
    procedure :: write3d
    procedure :: write4d
    procedure :: write5d
    procedure :: write6d
    procedure :: write7d
    generic   :: write =>write1d, write2d, write3d, write4d, write5d, write6d, write7d

    ! 读取过程
    procedure :: read1d 
    procedure :: read2d
    procedure :: read3d
    procedure :: read4d
    procedure :: read5d
    procedure :: read6d
    procedure :: read7d
    generic   :: read => read2d, read3d, read4d, read5d, read6d, read7d

    procedure :: close
  end type BinaryFile

contains
  ! 二维数组初始化
subroutine initialize1d(this, directory, file_tags, process_id, sample_data)
  class(BinaryFile), intent(out) :: this
  character(len=*), intent(in) :: directory
  character(len=*), intent(in), target :: file_tags(3)  ! 添加 target 属性
  integer, intent(in) :: process_id
  class(*), intent(in) :: sample_data(:)
  character(len=3), pointer :: tags(:)
  integer :: dir_stat
  tags => file_tags ! 使用指针避免临时数组
  this%file_path = trim(directory)//'/'//trim(tags(1))//trim(tags(2))//trim(tags(3))//'.dat'
  this%record_bytes = size(sample_data) * storage_size(sample_data)/8
  this%file_unit = 1000 + process_id
  open(unit=this%file_unit, file=this%file_path, form='unformatted', &
       access='direct', recl=this%record_bytes, status='unknown', iostat=dir_stat)
  if (dir_stat /= 0) then
    error stop "Failed to open file: "//trim(this%file_path)
  end if
  this%is_open =.true.
  ! 这里可以添加其他初始化代码 
  end subroutine
  subroutine initialize2d(this, directory, file_tags, process_id, sample_data)
    class(BinaryFile), intent(out) :: this
    character(len=*), intent(in) :: directory
    character(len=*), intent(in), target :: file_tags(3)  ! 添加 target 属性
    integer, intent(in) :: process_id
    class(*), intent(in) :: sample_data(:,:)
    character(len=3), pointer :: tags(:)
    integer :: dir_stat
    
    tags => file_tags  ! 使用指针避免临时数组
    this%file_path = trim(directory)//'/'//trim(tags(1))//trim(tags(2))//trim(tags(3))//'.dat'
    this%record_bytes = size(sample_data) * storage_size(sample_data)/8
    this%file_unit = 1000 + process_id
    
    open(unit=this%file_unit, file=this%file_path, form='unformatted', &
         access='direct', recl=this%record_bytes, status='unknown', iostat=dir_stat)
    if (dir_stat /= 0) then
      error stop "Failed to open file: "//trim(this%file_path)
    end if
    this%is_open = .true.
  end subroutine

  ! 三维数组初始化
  subroutine initialize3d(this, directory, file_tags, process_id, sample_data)
    class(BinaryFile), intent(out) :: this
    character(len=*), intent(in) :: directory
    character(len=*), intent(in), target :: file_tags(3)  ! 添加 target 属性
    integer, intent(in) :: process_id
    class(*), intent(in) :: sample_data(:,:,:)
    character(len=3), pointer :: tags(:)
    integer :: dir_stat
    
    tags => file_tags  ! 使用指针避免临时数组
    this%file_path = trim(directory)//'/'//trim(tags(1))//trim(tags(2))//trim(tags(3))//'.dat'
    this%record_bytes = size(sample_data) * storage_size(sample_data)/8
    this%file_unit = 1000 + process_id
    
    open(unit=this%file_unit, file=this%file_path, form='unformatted', &
         access='direct', recl=this%record_bytes, status='unknown', iostat=dir_stat)
    if (dir_stat /= 0) then
      error stop "Failed to open file: "//trim(this%file_path)
    end if
    this%is_open = .true.
  end subroutine

  ! 4D-7D 初始化子程序
  subroutine initialize4d(this, directory, file_tags, process_id, sample_data)
    class(BinaryFile), intent(out) :: this
    character(len=*), intent(in) :: directory
    character(len=*), intent(in), target :: file_tags(3)
    integer, intent(in) :: process_id
    class(*), intent(in) :: sample_data(:,:,:,:)
    character(len=3), pointer :: tags(:)
    integer :: dir_stat
    
    tags => file_tags
    this%file_path = trim(directory)//'/'//trim(tags(1))//trim(tags(2))//trim(tags(3))//'.dat'
    this%record_bytes = size(sample_data) * storage_size(sample_data)/8
    this%file_unit = 1000 + process_id
    
    open(unit=this%file_unit, file=this%file_path, form='unformatted', &
         access='direct', recl=this%record_bytes, status='unknown', iostat=dir_stat)
    if (dir_stat /= 0) then
      error stop "Failed to open file: "//trim(this%file_path)
    end if
    this%is_open = .true.
  end subroutine

  ! 5D initialization subroutine
  subroutine initialize5d(this, directory, file_tags, process_id, sample_data)
    class(BinaryFile), intent(out) :: this
    character(len=*), intent(in) :: directory
    character(len=*), intent(in), target :: file_tags(3)
    integer, intent(in) :: process_id
    class(*), intent(in) :: sample_data(:,:,:,:,:)
    character(len=3), pointer :: tags(:)
    integer :: dir_stat
    
    tags => file_tags
    this%file_path = trim(directory)//'/'//trim(tags(1))//trim(tags(2))//trim(tags(3))//'.dat'
    this%record_bytes = size(sample_data) * storage_size(sample_data)/8
    this%file_unit = 1000 + process_id
    
    open(unit=this%file_unit, file=this%file_path, form='unformatted', &
         access='direct', recl=this%record_bytes, status='unknown', iostat=dir_stat)
    if (dir_stat /= 0) then
      error stop "Failed to open file: "//trim(this%file_path)
    end if
    this%is_open = .true.
  end subroutine

  ! 6D initialization subroutine
  subroutine initialize6d(this, directory, file_tags, process_id, sample_data)
    class(BinaryFile), intent(out) :: this
    character(len=*), intent(in) :: directory
    character(len=*), intent(in), target :: file_tags(3)
    integer, intent(in) :: process_id
    class(*), intent(in) :: sample_data(:,:,:,:,:,:)
    character(len=3), pointer :: tags(:)
    integer :: dir_stat
    
    tags => file_tags
    this%file_path = trim(directory)//'/'//trim(tags(1))//trim(tags(2))//trim(tags(3))//'.dat'
    this%record_bytes = size(sample_data) * storage_size(sample_data)/8
    this%file_unit = 1000 + process_id
    
    open(unit=this%file_unit, file=this%file_path, form='unformatted', &
         access='direct', recl=this%record_bytes, status='unknown', iostat=dir_stat)
    if (dir_stat /= 0) then
      error stop "Failed to open file: "//trim(this%file_path)
    end if
    this%is_open = .true.
  end subroutine

  ! 7D initialization subroutine
  subroutine initialize7d(this, directory, file_tags, process_id, sample_data)
    class(BinaryFile), intent(out) :: this
    character(len=*), intent(in) :: directory
    character(len=*), intent(in), target :: file_tags(3)
    integer, intent(in) :: process_id
    class(*), intent(in) :: sample_data(:,:,:,:,:,:,:)
    character(len=3), pointer :: tags(:)
    integer :: dir_stat
    
    tags => file_tags
    this%file_path = trim(directory)//'/'//trim(tags(1))//trim(tags(2))//trim(tags(3))//'.dat'
    this%record_bytes = size(sample_data) * storage_size(sample_data)/8
    this%file_unit = 1000 + process_id
    
    open(unit=this%file_unit, file=this%file_path, form='unformatted', &
         access='direct', recl=this%record_bytes, status='unknown', iostat=dir_stat)
    if (dir_stat /= 0) then
      error stop "Failed to open file: "//trim(this%file_path)
    end if
    this%is_open = .true.
  end subroutine



  ! 写入/读取方法实现

  subroutine write1d(this, record_number, array_data) 
    class(BinaryFile), intent(in) :: this
    integer, intent(in) :: record_number
    class(*), intent(in) :: array_data(:)
    if (.not. this%is_open) error stop "File not open"
    select type (array_data)
      type is (real(4))
        write(this%file_unit, rec=record_number) array_data
      type is (real(8))
        write(this%file_unit, rec=record_number) array_data
      type is (complex(4))  ! 添加单精度复数支持
        write(this%file_unit, rec=record_number) array_data
      type is (complex(8))  ! 添加双精度复数支持
        write(this%file_unit, rec=record_number) array_data
      class default
        error stop "Unsupported data type"
    end select
    
  end subroutine

  subroutine write2d(this, record_number, array_data)
    class(BinaryFile), intent(in) :: this
    integer, intent(in) :: record_number
    class(*), intent(in) :: array_data(:,:)
    if (.not. this%is_open) error stop "File not open"
    
    select type (array_data)
      type is (real(4))
        write(this%file_unit, rec=record_number) array_data
      type is (real(8))
        write(this%file_unit, rec=record_number) array_data
      type is (complex(4))  ! 添加单精度复数支持
        write(this%file_unit, rec=record_number) array_data
      type is (complex(8))  ! 添加双精度复数支持
        write(this%file_unit, rec=record_number) array_data
      class default
        error stop "Unsupported data type"
    end select
  end subroutine

  subroutine write3d(this, record_number, array_data)
    class(BinaryFile), intent(in) :: this
    integer, intent(in) :: record_number
    class(*), intent(in) :: array_data(:,:,:)
    if (.not. this%is_open) error stop "File not open"
    
    select type (array_data)
      type is (real(4))
        write(this%file_unit, rec=record_number) array_data
      type is (real(8))
        write(this%file_unit, rec=record_number) array_data
      type is (complex(4))  ! 添加单精度复数支持
        write(this%file_unit, rec=record_number) array_data
      type is (complex(8))  ! 添加双精度复数支持
        write(this%file_unit, rec=record_number) array_data
      class default
        error stop "Unsupported data type"
    end select
  end subroutine

  ! 4D 写入/读取方法
  subroutine write4d(this, record_number, array_data)
    class(BinaryFile), intent(in) :: this
    integer, intent(in) :: record_number
    class(*), intent(in) :: array_data(:,:,:,:)
    if (.not. this%is_open) error stop "File not open"
    
    select type (array_data)
      type is (real(4))
        write(this%file_unit, rec=record_number) array_data
      type is (real(8))
        write(this%file_unit, rec=record_number) array_data
      type is (complex(4))  ! 添加单精度复数支持
        write(this%file_unit, rec=record_number) array_data
      type is (complex(8))  ! 添加双精度复数支持
        write(this%file_unit, rec=record_number) array_data
      class default
        error stop "Unsupported data type"
    end select
  end subroutine

  subroutine write5d(this, record_number, array_data)
    class(BinaryFile), intent(in) :: this
    integer, intent(in) :: record_number
    class(*), intent(in) :: array_data(:,:,:,:,:)
    if (.not. this%is_open) error stop "File not open"
    
    select type (array_data)
      type is (real(4))
        write(this%file_unit, rec=record_number) array_data
      type is (real(8))
        write(this%file_unit, rec=record_number) array_data
      type is (complex(4))  ! 添加单精度复数支持
        write(this%file_unit, rec=record_number) array_data
      type is (complex(8))  ! 添加双精度复数支持
        write(this%file_unit, rec=record_number) array_data
      class default
        error stop "Unsupported data type"
    end select
  end subroutine

  subroutine write6d(this, record_number, array_data)
    class(BinaryFile), intent(in) :: this
    integer, intent(in) :: record_number
    class(*), intent(in) :: array_data(:,:,:,:,:,:)
    if (.not. this%is_open) error stop "File not open"
    
    select type (array_data)
      type is (real(4))
        write(this%file_unit, rec=record_number) array_data
      type is (real(8))
        write(this%file_unit, rec=record_number) array_data
      type is (complex(4))  ! 添加单精度复数支持
        write(this%file_unit, rec=record_number) array_data
      type is (complex(8))  ! 添加双精度复数支持
        write(this%file_unit, rec=record_number) array_data
      class default
        error stop "Unsupported data type"
    end select
  end subroutine

  subroutine write7d(this, record_number, array_data)
    class(BinaryFile), intent(in) :: this
    integer, intent(in) :: record_number
    class(*), intent(in) :: array_data(:,:,:,:,:,:,:)
    if (.not. this%is_open) error stop "File not open"
    
    select type (array_data)
      type is (real(4))
        write(this%file_unit, rec=record_number) array_data
      type is (real(8))
        write(this%file_unit, rec=record_number) array_data
      type is (complex(4))  ! 添加单精度复数支持
        write(this%file_unit, rec=record_number) array_data
      type is (complex(8))  ! 添加双精度复数支持
        write(this%file_unit, rec=record_number) array_data
      class default
        error stop "Unsupported data type"
    end select
  end subroutine

! 读取方法 
  subroutine read1d(this, record_number, array_data)
    class(BinaryFile), intent(in) :: this
    integer, intent(in) :: record_number
    class(*), intent(out) :: array_data(:)
    if (.not. this%is_open) error stop "File not open"
    select type (array_data)
      type is (real(4))
        read(this%file_unit, rec=record_number) array_data
      type is (real(8))
        read(this%file_unit, rec=record_number) array_data
      type is (complex(4))  ! 添加单精度复数支持
        read(this%file_unit, rec=record_number) array_data
      type is (complex(8))  ! 添加双精度复数支持
        read(this%file_unit, rec=record_number) array_data
      class default
        error stop "Unsupported data type"
    end select
  end subroutine
  

  subroutine read2d(this, record_number, array_data)
    class(BinaryFile), intent(in) :: this
    integer, intent(in) :: record_number
    class(*), intent(out) :: array_data(:,:)
    if (.not. this%is_open) error stop "File not open"
    
    select type (array_data)
      type is (real(4))
        read(this%file_unit, rec=record_number) array_data
      type is (real(8))
        read(this%file_unit, rec=record_number) array_data
      type is (complex(4))  ! 添加单精度复数支持
        read(this%file_unit, rec=record_number) array_data
      type is (complex(8))  ! 添加双精度复数支持
        read(this%file_unit, rec=record_number) array_data
      class default
        error stop "Unsupported data type"
    end select
  end subroutine

  subroutine read3d(this, record_number, array_data)
    class(BinaryFile), intent(in) :: this
    integer, intent(in) :: record_number
    class(*), intent(out) :: array_data(:,:,:)
    if (.not. this%is_open) error stop "File not open"
    
    select type (array_data)
      type is (real(4))
        read(this%file_unit, rec=record_number) array_data
      type is (real(8))
        read(this%file_unit, rec=record_number) array_data
      type is (complex(4))  ! 添加单精度复数支持
        read(this%file_unit, rec=record_number) array_data
      type is (complex(8))  ! 添加双精度复数支持
        read(this%file_unit, rec=record_number) array_data
      class default
        error stop "Unsupported data type"
    end select
  end subroutine

  subroutine read4d(this, record_number, array_data)
    class(BinaryFile), intent(in) :: this
    integer, intent(in) :: record_number
    class(*), intent(out) :: array_data(:,:,:,:)
    if (.not. this%is_open) error stop "File not open"
    
    select type (array_data)
      type is (real(4))
        read(this%file_unit, rec=record_number) array_data
      type is (real(8))
        read(this%file_unit, rec=record_number) array_data
      type is (complex(4))  ! 添加单精度复数支持
        read(this%file_unit, rec=record_number) array_data
      type is (complex(8))  ! 添加双精度复数支持
        read(this%file_unit, rec=record_number) array_data
      class default
        error stop "Unsupported data type"
    end select
  end subroutine

  subroutine read5d(this, record_number, array_data)
    class(BinaryFile), intent(in) :: this
    integer, intent(in) :: record_number
    class(*), intent(out) :: array_data(:,:,:,:,:)
    if (.not. this%is_open) error stop "File not open"
    
    select type (array_data)
      type is (real(4))
        read(this%file_unit, rec=record_number) array_data
      type is (real(8))
        read(this%file_unit, rec=record_number) array_data
      type is (complex(4))  ! 添加单精度复数支持
        read(this%file_unit, rec=record_number) array_data
      type is (complex(8))  ! 添加双精度复数支持
        read(this%file_unit, rec=record_number) array_data
      class default
        error stop "Unsupported data type"
    end select
  end subroutine

  subroutine read6d(this, record_number, array_data)
    class(BinaryFile), intent(in) :: this
    integer, intent(in) :: record_number
    class(*), intent(out) :: array_data(:,:,:,:,:,:)
    if (.not. this%is_open) error stop "File not open"
    
    select type (array_data)
      type is (real(4))
        read(this%file_unit, rec=record_number) array_data
      type is (real(8))
        read(this%file_unit, rec=record_number) array_data
      type is (complex(4))  ! 添加单精度复数支持
        read(this%file_unit, rec=record_number) array_data
      type is (complex(8))  ! 添加双精度复数支持
        read(this%file_unit, rec=record_number) array_data
      class default
        error stop "Unsupported data type"
    end select
  end subroutine

  subroutine read7d(this, record_number, array_data)
    class(BinaryFile), intent(in) :: this
    integer, intent(in) :: record_number
    class(*), intent(out) :: array_data(:,:,:,:,:,:,:)
    if (.not. this%is_open) error stop "File not open"
    
    select type (array_data)
      type is (real(4))
        read(this%file_unit, rec=record_number) array_data
      type is (real(8))
        read(this%file_unit, rec=record_number) array_data
      type is (complex(4))  ! 添加单精度复数支持
        read(this%file_unit, rec=record_number) array_data
      type is (complex(8))  ! 添加双精度复数支持
        read(this%file_unit, rec=record_number) array_data
      class default
        error stop "Unsupported data type"
    end select
  end subroutine

  subroutine close(this)
    class(BinaryFile), intent(inout) :: this
    if (this%is_open) then
      close(this%file_unit)
      this%is_open = .false.
    end if
  end subroutine

end module