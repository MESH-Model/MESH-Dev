!
! Successive calls to the following macros generate
! two common blocks:
!
!    * MARK_COMPHY_BEG: marks the beginning of the common block
!      of "pointers" (of type INTEGER) that define the structure
!      of the bus
!    * MARK_COMPHY_END: marks the end of the same common block
!    * DCL_PHYVAR: this macro has to be called for each variable
!      included in the bus. If DCLCHAR is not defined, then only
!      the common block of "pointers" is declared. If DCLCHAR is
!      defined, then both the common block of "pointers" and the
!      common block of the corresponding "names" (of type CHARACTER)
!      are declared. If DATACHAR is defined, these "names" are
!      defined via an appropriate data statement
!
! Example:
!       SUBROUTINE BIDON
! #define DCLCHAR
! #include "phy_macros_f.h"
!       MARK_COMPHY_BEG (phybus)           ! Begins the common block "phybus"
!       DCL_PHYVAR( AL        ,phybus)     ! Adds one "pointer" to the common block
!       DCL_PHYVAR( MG        ,phybus)
!       ...
!       DCL_PHYVAR( Z0        ,phybus)
!       MARK_COMPHY_END (phybus)           ! Ends the common block "phybus"
!       equivalence (phybus_i_first(1),AL) ! "pointer" AL is now the first element
!                                            of the common block "phybus"
!       ...
!       return
!       end
!
! For details of implementation, see comdeck "phybus.cdk"
! and subroutine "phy_ini.ftn" in the physics library.
!
! Author : Vivian Lee (Nov 2000) - adapted by B. Bilodeau
!
! Revisions:
! 002  Dugas, B. (Aug 2005) - add DATACHAR and modifiy DCLCHAR
!

#define _cat_(name1,name2) name1##name2

#define _cat3_(name1,name2,name3) name1##name2##name3

#define AUTOMATIC(name,type,dims) ~~\
type _cat_(name,dims)

#if defined (DATACHAR)
#define DCLCHAR
#endif

#ifndef DCLCHAR

#define DCL_PHYVAR(__TOKEN__,_COM_)~~\
integer __TOKEN__~~\
common/_cat_(_COM_,_i)/__TOKEN__~~\

#define DCL_PHYVAR2(__TOKEN__,__NAME__,_COM_)~~\
integer __TOKEN__~~\
common/_cat_(_COM_,_i)/__TOKEN__~~\

#define MARK_COMPHY_BEG(_COM_) ~~\
integer _cat3_(_COM_,_i,_first(-1:0))~~\
common /_cat_(_COM_,_i)/_cat3_(_COM_,_i,_first)

#else

#ifndef DATACHAR

#define DCL_PHYVAR(__TOKEN__,_COM_)~~\
integer __TOKEN__~~\
character*16 _cat_(__TOKEN__,_c)~~\
common/_cat_(_COM_,_i)/__TOKEN__~~\
common/_cat_(_COM_,_c)/_cat_(__TOKEN__,_c)

#define DCL_PHYVAR2(__TOKEN__,__NAME__,_COM_)~~\
integer __TOKEN__~~\
character*16 _cat_(__TOKEN__,_c)~~\
common/_cat_(_COM_,_i)/__TOKEN__~~\
common/_cat_(_COM_,_c)/_cat_(__TOKEN__,_c)

#define MARK_COMPHY_BEG(_COM_)~~\
integer _cat3_(_COM_,_i,_first(-1:0))~~\
common /_cat_(_COM_,_i)/_cat3_(_COM_,_i,_first)~~\
character*16 _cat3_(_COM_,_c,_first(-1:0))~~\
common /_cat_(_COM_,_c)/_cat3_(_COM_,_c,_first)

#else

#define DCL_PHYVAR(__TOKEN__,_COM_)~~\
integer __TOKEN__~~\
character*16 _cat_(__TOKEN__,_c)~~\
data  _cat_(__TOKEN__,_c) /'__TOKEN__'/~~\
common/_cat_(_COM_,_i)/__TOKEN__~~\
common/_cat_(_COM_,_c)/_cat_(__TOKEN__,_c)

#define DCL_PHYVAR2(__TOKEN__,__NAME__,_COM_)~~\
integer __TOKEN__~~\
character*16 _cat_(__TOKEN__,_c)~~\
data  _cat_(__TOKEN__,_c) /__NAME__/~~\
common/_cat_(_COM_,_i)/__TOKEN__~~\
common/_cat_(_COM_,_c)/_cat_(__TOKEN__,_c)

#define MARK_COMPHY_BEG(_COM_)~~\
integer _cat3_(_COM_,_i,_first(-1:0))~~\
common /_cat_(_COM_,_i)/_cat3_(_COM_,_i,_first)~~\
character*16 _cat3_(_COM_,_c,_first(-1:0))~~\
common /_cat_(_COM_,_c)/_cat3_(_COM_,_c,_first)

#endif

#endif

#define MARK_COMPHY_END(_COM_)~~\
integer _cat3_(_COM_,_i,_last)~~\
common /_cat_(_COM_,_i)/_cat3_(_COM_,_i,_last)

#define COMPHY_SIZE(_COM_) (loc(_cat3_(_COM_,_i,_last))-\
loc(_cat3_(_COM_,_i,_first(0)))-1)/(loc(_cat3_(_COM_,_i,_first(0)))-\
loc(_cat3_(_COM_,_i,_first(-1))))

