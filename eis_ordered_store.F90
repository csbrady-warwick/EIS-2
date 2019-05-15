MODULE eis_ordered_store_mod

  IMPLICIT NONE

  INTEGER, PARAMETER :: INT32 = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: INT64 = SELECTED_INT_KIND(15)

  TYPE :: ordered_store_item
    CLASS(*), POINTER :: item => NULL()
    CONTAINS
    PROCEDURE :: cleanup => osi_cleanup
    FINAL :: osi_destructor
  END TYPE ordered_store_item

  TYPE :: ordered_store
    PRIVATE
    TYPE(ordered_store_item), DIMENSION(:), ALLOCATABLE :: items
    LOGICAL, PUBLIC :: is_init = .FALSE.
    CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC :: get_size => os_get_size
    PROCEDURE, PUBLIC :: get => os_get
    PROCEDURE, PUBLIC :: store => os_store
    FINAL :: os_destructor
  END TYPE ordered_store

  PRIVATE
  PUBLIC :: ordered_store

CONTAINS


  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Deallocates the item held in the store
  !> @param[inout] this
  PURE ELEMENTAL SUBROUTINE osi_cleanup(this)
    CLASS(ordered_store_item), INTENT(INOUT) :: this !< self pointer

    IF (ASSOCIATED(this%item)) DEALLOCATE(this%item)
    this%item => NULL()
  END SUBROUTINE osi_cleanup



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Unlinks but does not deallocate the associated item
  !> @param[inout] this
  PURE ELEMENTAL SUBROUTINE unlink_items(item_store)
    CLASS(ordered_store_item), INTENT(INOUT) :: item_store !< itemstore

    item_store%item => NULL()
  END SUBROUTINE unlink_items



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Destructor for the item store. Deallocates the item held in the store
  !> @param[inout] this
  PURE ELEMENTAL SUBROUTINE osi_destructor(this)
    TYPE(ordered_store_item), INTENT(INOUT) :: this !< self pointer

    CALL this%cleanup()
  END SUBROUTINE osi_destructor



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get the number of items in this store. Returns 0 if empty
  !> @param[in] this
  !> @result item
  FUNCTION os_get_size(this) RESULT (sz)

    CLASS(ordered_store), INTENT(IN) :: this
    INTEGER :: sz

    sz = 0
    IF (.NOT. ALLOCATED(this%items)) RETURN
    sz = SIZE(this%items)

  END FUNCTION os_get_size



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Get an item from the store by index
  !> return NULL pointer if item is not present
  !> @param[in] this
  !> @param[in] index
  !> @result item
  FUNCTION os_get(this, index) RESULT (item)

    CLASS(ordered_store), INTENT(IN) :: this
    INTEGER(INT32), INTENT(IN) :: index
    CLASS(*), POINTER :: item

    item => NULL()
    IF (.NOT. ALLOCATED(this%items)) RETURN
    IF (index < 1 .OR. index > SIZE(this%items)) RETURN

    item => this%items(index)%item

  END FUNCTION os_get



  !> @author C.S.Brady@warwick.ac.uk
  !> @brief
  !> Add an item to the ordered store
  !> @param[in] this
  !> @param[in] item
  !> @result os_store
  FUNCTION os_store(this, item, index, nocopy)

    CLASS(ordered_store), INTENT(INOUT) :: this
    CLASS(*), TARGET, INTENT(IN) :: item
    INTEGER, INTENT(IN), OPTIONAL :: index
    LOGICAL, INTENT(IN), OPTIONAL :: nocopy
    INTEGER(INT32) :: os_store
    TYPE(ordered_store_item), DIMENSION(:), ALLOCATABLE :: temp
    INTEGER(INT32) :: sz
    LOGICAL :: makecopy
    INTEGER(INT32) :: re_store_index

    makecopy = .TRUE.
    IF (PRESENT(nocopy)) makecopy= .NOT. nocopy

    IF (PRESENT(index)) THEN
      IF (ALLOCATED(this%items)) THEN
        IF (index >=1 .OR. index <= SIZE(this%items)) THEN
          CALL this%items(index)%cleanup()
          IF (makecopy) THEN
            ALLOCATE(this%items(index)%item, SOURCE = item)
          ELSE
            this%items(index)%item => item
          END IF
          os_store = index
          RETURN
        END IF
      END IF
    END IF

    makecopy = .TRUE.
    IF (PRESENT(nocopy)) makecopy= .NOT. nocopy

    IF (.NOT. ALLOCATED(this%items)) THEN
      sz = 1
      ALLOCATE(this%items(1))
    ELSE
      sz = SIZE(this%items)
      ALLOCATE(temp(1:sz+1))
      temp(1:sz) = this%items
      CALL unlink_items(this%items)
      DEALLOCATE(this%items)
      CALL MOVE_ALLOC(temp, this%items)
      sz = sz + 1
    END IF

    IF (makecopy) THEN
      ALLOCATE(this%items(sz)%item, SOURCE = item)
    ELSE
      this%items(sz)%item => item
    END IF
    os_store = sz

  END FUNCTION os_store



  !> Delete all stored items on destruction
  PURE ELEMENTAL SUBROUTINE os_destructor(this)

    TYPE(ordered_store), INTENT(INOUT) :: this

    IF (.NOT. ALLOCATED(this%items)) RETURN
    DEALLOCATE(this%items)

  END SUBROUTINE os_destructor

END MODULE eis_ordered_store_mod
