program test_io
  use binary_file_io
  implicit none

  type(BinaryFile) :: data_file
  real(4), allocatable :: temperature(:,:), temp_read(:,:)
  real(8), allocatable :: velocity(:,:,:), vel_read(:,:,:)
  integer :: i, j, k
  character(len=3), parameter :: tags_2d(3) = ["TMP", "001", "DAT"]
  character(len=3), parameter :: tags_3d(3) = ["VEL", "002", "DAT"]
  character(len=*), parameter :: output_dir = "./output"
  integer :: iostat
  character(len=256) :: msg
  logical :: dir_exists

  ! 替换目录检查部分
  call execute_command_line("mkdir -p "//output_dir, exitstat=iostat)
  ! 检查目录是否已存在
  ! inquire(file=trim(output_dir)//'/.', exist=dir_exists)
  ! if (.not. dir_exists) then
  !   call execute_command_line("mkdir -p "//output_dir, exitstat=iostat)
  !   if (iostat /= 0) then
  !     print *, "错误：无法创建输出目录"
  !     stop
  !   end if
  !   ! 再次检查目录是否创建成功
  !   inquire(file=trim(output_dir)//'/.', exist=dir_exists)
  !   if (.not. dir_exists) then
  !     print *, "错误：输出目录创建失败"
  !     stop
  !   end if
  ! end if

  ! 测试二维数组
  allocate(temperature(100, 200))
  temperature = reshape([(((i*0.1_4 + j*0.01_4), i=1,100), j=1,200)], [100,200])

  call data_file%initialize(output_dir, tags_2d, 5, temperature)
  call data_file%write(1, temperature)

  allocate(temp_read, mold=temperature)
  call data_file%read(1, temp_read)
  print*, "Test 2D:", merge("Passed", "Failed", all(abs(temperature - temp_read) < 1e-6))
  call data_file%close()

  ! 测试三维数组
  allocate(velocity(50, 60, 70))
  velocity = reshape([((((i + j + k)*0.01_8, i=1,50), j=1,60), k=1,70)], [50,60,70])

  call data_file%initialize(output_dir, tags_3d, 10, velocity)
  call data_file%write(1, velocity)

  allocate(vel_read, mold=velocity)
  call data_file%read(1, vel_read)
  print*, "Test 3D:", merge("Passed", "Failed", all(abs(velocity - vel_read) < 1e-12))
  call data_file%close()

  deallocate(temperature, temp_read)
  deallocate(velocity, vel_read)

end program