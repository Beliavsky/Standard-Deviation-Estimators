program xstdev
use kind_mod    , only: dp
use ziggurat_mod, only: zigset, rnor
use stdev_mod   , only: mean,sd
implicit none
integer      , parameter :: ntotal = 10**7, ngroups = ntotal/100, n = ntotal/ngroups, &
                            nmethods = 4, niter = 10
real(kind=dp), parameter :: true_sd = 1.0_dp
logical      , parameter :: print_each = .false.
real(kind=dp)            :: x(ntotal),xsd(ngroups,nmethods),avg_err(niter,nmethods)
integer                  :: i,i1,i2,imethod,iter
print*,"#total, #groups, group_size =",ntotal,ngroups,n
call zigset(123456)
do iter=1,niter
   x = rnor(ntotal)
   x = x - mean(x)        ! set mean to zero
   x = x/sqrt(mean(x**2)) ! set variance to one
   i1 = 1
   do i=1,ngroups
      i2 = i1 + n - 1
      forall (imethod=1:4) xsd(i,imethod) = sd(x(i1:i1+n-1),imethod) ! compute sd for subset using various methods
      i1 = i1 + n
   end do
   if (print_each) write (*,"(/,2a10)") "method","avg_error"
   do imethod=1,nmethods
      avg_err(iter,imethod) = mean(abs(xsd(:,imethod)-true_sd)) ! average absolute error of sd estimates for subsets
      if (print_each) write (*,"(i10,f10.6)") imethod,avg_err(iter,imethod)
   end do
end do
write (*,"(/,'OVERALL',/,2a10)") "method","avg_error"
do imethod=1,nmethods
   write (*,"(i10,f10.6)") imethod,mean(avg_err(:,imethod))
end do
end program xstdev
