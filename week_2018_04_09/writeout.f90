program main_project
    implicit none
    integer(kind=4) :: timeStamp, result
    real(kind=8) :: sample(4, 3)
    timeStamp = time()
    sample(1, :) = [1.0, 2.0, 3.0]
    sample(2, :) = [2.0, 5.0, 3.0]
    sample(3, :) = [3.0, 8.0, 3.0]
    sample(4, :) = [4.0, 11.0, 3.0]
    print*, timeStamp
    ! print*, sample
    result = whiteOut(sample, timeStamp)
    contains
    function whiteOut(dataVals, fileNumber) result(status)
      real(kind=8), intent(in) :: dataVals(:, :)
      integer(kind=4), intent(in) :: fileNumber
      character(len=20) :: fileName
      integer :: status, lineCount
      write(fileName, '(i10, a4)') fileNumber, '.txt'
      ! print*, trim(fileName)
      open(1, file=fileName)
      do lineCount=1,size(dataVals,1)
        print *, lineCount, dataVals(lineCount, :)
        write(1, *) dataVals(lineCount, :)
      end do
      close(1)
      status = 4
    end function whiteOut
end program main_project

