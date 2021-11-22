module stdev_mod
use kind_mod, only: dp
implicit none
private
public :: mean,sd
contains
pure function mean(x) result(xmean)
real(kind=dp), intent(in) :: x(:)
real(kind=dp)             :: xmean
xmean = sum(x)/max(1,size(x))
end function mean
!
pure function sd(x,imethod) result(xsd)
! formulas to compute standard deviation from Wikipedia https://en.wikipedia.org/wiki/Standard_deviation
real(kind=dp), intent(in) :: x(:)
integer      , intent(in) :: imethod
real(kind=dp)             :: xsd
real(kind=dp)             :: xmean,ssd
integer                   :: n
n = size(x)
if (n < 2) then
   xsd = -1.0_dp
   return
end if
xmean = mean(x)
ssd   = sum((x-xmean)**2)
select case (imethod)
   case (1)    ; xsd = sqrt(ssd/n)
   case (2)    ; xsd = sqrt(ssd/(n - 1))
   case (3)    ; xsd = sqrt(ssd/(n - 1.5_dp))
   case (4)    ; xsd = sqrt(ssd/(n - 1.5_dp + 1.0_dp/(8*n-8)))
   case default; xsd = -2.0_dp
end select
end function sd
end module stdev_mod
