program sistema_triangular_superior_lapack
   implicit none
   integer :: n, info, i, j
   real(8), allocatable :: A(:,:), b(:)
 
   ! Declarar a rotina LAPACK dtrsv
   external :: dtrsv
 
   ! Definir o tamanho do sistema
   print *, "Entre com o tamanho do sistema (n):"
   read(*,*) n
 
   ! Alocar memória para a matriz e o vetor
   allocate(A(n,n), b(n))
 
   ! Ler a matriz A (triangular superior)
   print *, "Entre com a matriz A (triangular superior):"
   do i = 1, n
      do j = 1, n
         if (j >= i) then
            read(*,*) A(i,j)
         else
            A(i,j) = 0.0 ! Elementos abaixo da diagonal são zero
         end if
      end do
   end do
 
   ! Ler o vetor b
   print *, "Entre com o vetor b:"
   do i = 1, n
      read(*,*) b(i)
   end do
 
   ! Chamar a rotina LAPACK DTRSV para resolver o sistema
   call dtrsv('U', 'N', 'N', n, A, n, b, 1)
 
   ! Exibir a solução (vetor b agora contém a solução x)
   print *, "A solução do sistema é:"
   do i = 1, n
      print *, "x(", i, ") = ", b(i)
   end do
 
   ! Liberar a memória alocada
   deallocate(A, b)
 
 end program sistema_triangular_superior_lapack
 
  