subroutine baseflow_wfqlz(WF_LZFA, WF_LZFPWR, WF_RCHRG, WF_QLZ, NML, IL1, IL2)

    integer, intent(in) :: NML, IL1, IL2
    real, intent(in) :: WF_LZFA(NML), WF_LZFPWR(NML), WF_RCHRG(NML)
    real, intent(out) :: WF_QLZ(NML)

    WF_QLZ = WF_LZFA*WF_RCHRG**WF_LZFPWR

end subroutine
