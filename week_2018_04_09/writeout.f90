program main_project
    implicit none
    integer(kind=4) ::  result
    real(kind=8) :: sample(4, 3)
    sample(1, :) = [1.0, 2.0, 3.0]
    sample(2, :) = [2.0, 5.0, 3.0]
    sample(3, :) = [3.0, 8.0, 3.0]
    sample(4, :) = [4.0, 11.0, 3.0]
    result = writeOutAtTime(sample)
    contains
    function writeOutAtTime(dataVals) result(status)
      real(kind=8), intent(in) :: dataVals(:, :)
      integer(kind=4) :: fileNumber
      character(len=20) :: fileName
      integer :: status, lineCount
      fileNumber = time()
      write(fileName, '(i10, a4)') fileNumber, '.txt'
      print*, trim(fileName)
      open(1, file=fileName)
      do lineCount=1,size(dataVals,1)
        write(1, *) dataVals(lineCount, :)
      end do
      close(1)
      status = 4
    end function writeOutAtTime
end program main_project

!Xc7 Xc6 xg7 Xc5 c2
