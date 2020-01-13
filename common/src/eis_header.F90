MODULE eis_header

  USE eis_constants
  USE eis_utils
  USE eis_algorithm
  IMPLICIT NONE

  CONTAINS

  !> @brief
  !> Returns the version as a string
  !> @param[out] str
  !> @param[in] level
  SUBROUTINE eis_get_version_string(str, level)
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: str
    INTEGER, INTENT(IN), OPTIONAL :: level
    INTEGER :: ilevel, i
    INTEGER :: vnums(3)
    CHARACTER(LEN=:), ALLOCATABLE :: v

    ilevel = 3
    IF (PRESENT(level)) ilevel = level
    IF (ilevel < 1) ilevel = 1
    IF (ilevel > 3) ilevel = 3
    CALL eis_get_version_numbers(vnums(1), vnums(2), vnums(3))
    DO i = 1, ilevel
      CALL eis_integer_as_string(vnums(i), v)
      CALL eis_append_string(str, v, newline = .FALSE.)
      IF (i /= ilevel) CALL eis_append_string(str, '.', newline = .FALSE.)
    END DO

  END SUBROUTINE eis_get_version_string



  !> @brief
  !> Returns a string detailing the version of EIS being used
  !> If needed other copyright information will be returned here
  !> @param[out] str
  !> @param[in] level
  SUBROUTINE eis_get_version_report(str, level)
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: str
    INTEGER, INTENT(IN), OPTIONAL :: level
    CHARACTER(LEN=:), ALLOCATABLE :: vstring

    CALL eis_get_version_string(vstring, level)
    CALL eis_append_string(str, 'Using EIS-2 parser version ' // vstring)

  END SUBROUTINE eis_get_version_report

END MODULE eis_header
