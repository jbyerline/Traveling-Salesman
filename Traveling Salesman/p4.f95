!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! PROGRAM P4
!             Traveling Salesman Problem in FORTRAN 95
!
! Jacob Byerline CSSC0457 10/28/2019
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

PROGRAM P4

    INTEGER :: numberOfCities, fileStatus, I, xDim, yDim, permutation = 0, distance = 0, best_distance = 1000000, count = 0
    CHARACTER(50) :: filename
    CHARACTER(20), DIMENSION(10) :: cityName
    INTEGER, DIMENSION(10,10) :: dataTable
    INTEGER, DIMENSION(100) :: route, best_route

    !Prompts user to enter file name and recieves name
    WRITE (*, '(1x,A)', ADVANCE="NO")"Enter filename:  "
    PRINT *
    READ *, filename
    PRINT *

    !Open the file and read the contents
    OPEN(UNIT=15, FILE=filename, STATUS="OLD", ACTION="READ",&
     IOSTAT=fileStatus)
    !If errror opening file, print error
    IF(fileStatus /= 0) THEN
        PRINT *, "ERROR, could not open file for reading."
        STOP
    END IF

    !Read first line of file into Number of Cities Variable
    read(15,*) numberOfCities
    !Creates 2D Array with nested Do loops
    do xDim = 1, numberOfCities
        route(xDim) = xDim
        ! ORIGIONALLY (xDim-1)
        !Reads in second dim of array
        read(15,*, IOSTAT=fileStatus) cityName(xDim)
        !Stop at EOF
        IF(status < 0) THEN
            EXIT
        END IF
        do yDim= 1, numberOfCities
            !Reads in second dim of array
            read(15, *) dataTable(xDim,yDim)
        end do
    end do
    !Close file
    CLOSE(15)

    !Call permute function
    CALL permute(2,numberOfCities)

    !Prints formtted output
    DO iter = 1, numberOfCities - 1
       print *, cityName(best_route(iter)), " ", cityName(best_route(iter+1)), &
            "-- " ,dataTable(best_route(iter),best_route(iter+1))
    ENDDO
    print *, cityName(best_route(numberOfCities)), " ", cityName(best_route(1)), &
            "-- ", dataTable(best_route(numberOfCities),best_route(1))
    PRINT *
    print *, "Best distance is: ", best_distance, "miles"


    CONTAINS
    !Defines permute function
    RECURSIVE SUBROUTINE permute(first, last)
        INTEGER, INTENT(IN) :: first, last
        INTEGER :: i, temp

            IF(first == last)  THEN

                !Loops through distances, counting loops
                distance = dataTable(1,route(2))
                    !PRINT *, cityName(1), cityName(route(2)), " ", dataTable(1, route(2))
                DO i=2, last-1
                    !Incriments counter variable
                    count = count + 1
                    distance = distance + dataTable(route(i),route(i+1))
                        !print *, cityName(route(i)), " ", cityName(route(i+1)), dataTable(route(i),route(i+1))
                END DO
                distance = distance + dataTable(route(last),route(1))
                    !PRINT *, cityName(route(last))," ",cityName(route(1)), dataTable(route(last),route(1))
                    !PRINT *, "Distance is ",distance
                    !PRINT *
                !Keeps track of number of permutations
                permutations = permutations + 1

                !Checks if distance found is new best
                !If so, sets as new best
                IF(distance < best_distance) THEN
                    best_distance = distance
                    DO i=1, last
                        best_route(i) = route(i)
                    END DO
                END IF

            ELSE
                
                !Mixes up permutations to check different combo
                DO i=first, last
                    temp = route(first)
                    route(first) = route(i)
                    route(i) = temp
                    call permute(first+1,last)
                    temp = route(first)
                    route(first) = route(i)
                    route(i) = temp
                END DO
            END IF
    END SUBROUTINE permute
    
END PROGRAM p4
