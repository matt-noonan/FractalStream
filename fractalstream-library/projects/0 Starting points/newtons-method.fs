script: |
  @common

  # find the roots of f with Newton's method
  f(z) := z^3 - 1

  # one step of Newton's method
  newton(z) := z - f(z) / f'(z)

  @viewer "Newton's method on ℂ" colorizer="Rainbow(arg z / (2π))"
  lastZ := 0
  repeat:
    lastZ <- z
    z <- newton(z)
  until z = lastZ

  @tool "Trace" shortcut=t
  on refresh:
    erase
  on deactivated:
    erase
  on click-or-drag:
    use white for line
    erase
    prevZ := z
    repeat:
      prevZ <- z
      draw point at z
      z <- newton(z)
      draw line from z to prevZ
    until z = prevZ
metadata:
  title: Newton's method
  description: |
    Newton's method for a function of your choosing, coloring each point by
    the root it converges to. Edit f(z) at the top of the script, and pick
    other colors with the viewer's colorizer.
  preview-commands: |
    capture viewer "Newton's method on ℂ" as preview
  preview: |
    iVBORw0KGgoAAAANSUhEUgAAAIAAAACACAYAAADDPmHLAAARWklEQVR4nO1du3JcyQ2dn3BiJ44d
    uYpWbKdi5ECKHZlVZOx4uR+xCRMpXv7JVq2y/R7RBDWHi4tBAwfoHpJbugGKnPvoudM4eDSAiz58
    +PXwYOnjl5g+KHo+5ozDjste371GX/fhy/j+8PnJ35jRbxeHh08X7x7+dvitTP+4+Pw8zj+vbh4e
    DgeavjrHfnykQ4dhVUZmY64CADNOF7xVsI3O3VzcPPzvqg6A/xw+P92HcS7v3pUA4IEhBUAHCKsn
    MrpuRkOUv5v4bczvv707PHxuaAC554e7b88hf385vAulW58bnacB8ERGamZAwN6z8vzK55ohAUBF
    A4jkQ/pF7eN/AOArwegRMG5LADgzCM4NAPb3lJnauIfVAMJsAYzcc3/1TerlXvlrmZ19HmqAEuPM
    +RkQnBsA1edYLekR/fzIzMzZE2kXZstfuYdx7r4WmD8EwHDiAjt4LhCc49wJ41nz0QXJl+2YGQDE
    SRQSxsuKQajj4UfnvrIA2Pz4IjMrTGCkfCUARr+t+ywjho/I8wFE6oXxovIZhrM2PzINNAAqanIG
    BCsA0DUbK80AHD1R4dePTH1/902Vw4uXa8D4nw43TxpBe/UjJjKSnoHiBAAsEysTsBIEq44z4Eg1
    RQIYWZsL44Wporo9ZggoBCDi4FVtdsfWW01BAeBEpTWkowKCjjqvSn/K+AnAg0TKReK9iQcJMIT5
    IvWWkZ667qh5O+Z8HGBiUmZB0AFAlfkzTAeJ5AtFDBLNANUPjZEFdL4eYolnj3lj3lY0wEqTUNIC
    3vEFoMie6+Rc8puFueLIeQzA+t3G9GXc9wYEmbrPADGS+lYuwIubd4FQAcFmsi0T1PPYJVaX+fY4
    81vsMTDXTrYwXhhuAzz6XkkOMcu9GfIA8aQBRozt0DQIBsxmwMIyc9bOj8YXx04YLYyEVy/LOst8
    DwBCmfmIpL3C9LMCoAOEEQDaJsMCidQIswARVS7MF0dQmCnMHwV7dFYPJPdlPsBqLVCOA6wEwol9
    ddR5GwC/qnEJ5p88S1ELwI6zMX4PAIgbsNLbWQLacfhlIMHEaRAE41YBYIGVXZd9vwcMfR/Udxbj
    B8l19lmYFQEDjuzemhOYAKACiNHYMxLOnuuYggohbu/Ze48kVmCfDbGBrtofBXtGgKgtAxuTQgGh
    oF1YAFRNQ+u5HdJr/IwELPdHLSDmA5FBuxys5verPoGrATo/vgqGk+UccV8VAEPmN8CWkUiuMLVS
    3SP3CPMRF8ikP5LwLiD4bOAkADLpYkDAHO84hayfEl0jjLwOvP6RI6j/n40DtDXAkCGaSKmkqAiC
    zFHrgGUlqAEAUd8s4+X6+6tT5o2knPEJmDFCAEQMY8vAKHAMpLDq8I2OdSQ4fQZ7zHxGkWbF+dNL
    vo5jVzk2uqaUDawCYKhRGgxgAFAyCRlQGxouCvzAzqOw83MQ+vWKPVj1Hq3727mAWcZXbX1FjVc+
    VzRCqgEcEsZGki9a4v7oLGrms9U9WerYA8DXYPzhMrCl1glix6BtfmLXKyBa4RMIgz3my3Hx8GWZ
    GDGVYTBjFhit4AKgo7rLRI4TgWD0OQPDCiZHdK+igKLusbTr2mmPgczycFRU4l33BIDVkp4BgP0e
    9xp9bPS/A4bM1oe/X31Gxg9xe8T/hdHQACgHYxw7xnZXKFL3VDp4RnLP6RswAHrWVoHGmNUIEq0b
    qW6x85USbtbmV8dgw8Dy93kZyDJ2JcMrQLDMZcyDB9rIL9iYPOeekeeOCCBy/NHyboWkZ55+BRTh
    KsD1AyYkuao9ImaOnLkRs1eYNZFw7cjJJNr6flxrvfwZtR4BgAFU9J1pLuBkIl7IX2Cc0AgAm8+B
    1FecW4R7xd6LlFvm6yofFIdkTGA1wjmSQu4ykFHFHca3wZLZ/IK0z4JVGI939oTZdrmHd/i0xqjY
    85ciDbpnDcA6Y6zUuCp9kjERCCIArNBSyNRF+f57U+TBAmAFCKYKQlipbzE9kejUnyg4ch4QvM+R
    Rhkdw9IvS/Hqe0dFnppBs5ohWvYx35smg0bnookNgfIC/kNV6plrmWofFHlcHt8DxDv8bEInigSO
    AMN6/akGiKTZnbAvNWa2mb4AMKGGIZ9BvP8o1g8nEAkhAUtU39cN8nTGiLTB7aGwCmCPVc6zzBqB
    cGRmVmsUpqHD5SDHX1HNM4DomI8TAKxifnYuYvpq5jHPMAsAUf9y3b1T1hWt26vmYTUINquACqM7
    Ur9x7EimZ8/COqczL7eCsSPm40WQite/0jR4Y458A1cDZEuojPkWPCHzCyB6/nwGzUBpLnWNtwIQ
    my/Mv1RBn5de089I/0YDVCSwyij2WPVamtFFjTEiK/VI/nSidLPRPi3pHRC4uYCTySmq3xnTwdr/
    WYBlJLYeHT5kXa/X9kj12mQPY8fZfACbKVyhbTZOoMfYtsomma9VbeW7StJfAIat2tGMAxiYtXqV
    gYyDx9h09rvdUPDM5Lr/F0wHzXxSlVfBA7CKTffSvbD/cq129NjJrjKxew2TcLIO4kkgaCT9o8ln
    PXL2ng4YVmiN22M1j55E29gB10Yh3o6Kno0BVM2Gvo4qCGEmP2QEM74DPAqYyXdYJzW6BvZeTIGN
    +tmGDlFLF4bJVedtJpkUmaatCZj1tpvXdSW3o4k8ujmq9VE3D6sBhN4PWrVHMfsKGNiYf0XyveOx
    D9BgTCX6Vx2z4kewJKofjl/E/JuLba7/xqn/yxjJOntdLcCo/BAALxWG1eAqfWfnHoK8Jk6WBCQi
    9QAN/IBRiDfTCl0Pn2U6A5hhMuit0EsAEi3Ys3f6UA0s9yD54zGvsxRktUTFqWSvL0cCZ6T9LTEe
    VNm+BRs1ZHX/DBhGiaJsnGyMqpYobRnzGgya/a7s/kpTB7leN3HoSHJFG1Q1SUUzUABA9EuWRCiI
    eC2GZiFkS+y4ItFMXx84gT8P8v0e82YYx4SXqwDzNM6tBwC8wSoTgyoXDLC59szSyY7RSRHjOlYD
    2HQvK71dxs2s+yMguXEAPWGwcbbsGQ2M0P8ex364qzFLxocXfXl32iuPpUvlkQsQ5e/7xnhZYye8
    4DmS/JG0Ml5+NRjUlfzIz9iYAHSqitSgvQYmwut7Z7WESBvAJf8LoKBWnxonEjEEAAiJGRnn8tho
    CW3WnjdmCMbB/9kKQMayzZxZ5lTiApU4fpX50fkNAKKyJ4RCMRHohytfIFIijN0w3Uy252lfH/fG
    wfjYSGGkwj2A4rvxOraMqbdY8zpyagBkGgCpX3aNn0n7Snu+gjYAGHW5Qk28TZXqgfQmCAzjhIQJ
    +n4BFxjpMc22YAEoRxOKpdomjm/A5QFAvh8vgp4zLu9JeUeKZ4C0AYBn99HPhh1QJvSTeUHCK6cW
    Zo4SKnIcql6P4zFK7583UqkAFzRMBAC8+1eZxCjw0g0Ps8z2fA3mO91loFWrzMuNHsl9OnauJ9mL
    oY/GF6bBsdMmBOZi5HxFEiVjaucTzwap9yaXDa3OMJQFiV3L2987Wo561z0DAGlQbxK6EwAHTVSu
    TLBMtLfbZTaB8llvmbpicoWwDy9KvLLEzmrbzcYMPKZlIGfG3wBA/vFq3bIHYiYCW6LNMEuYE7VV
    Gz1LxiAB5E/Fos4ug98qPQEAkTCsr6OiR/YznEIZF3ZX73jNagL030cUEj15Z5ivs3/o1xsFeaoM
    Zu1+Z1l3FgDAttolmC2EZCYA0qrDq3Yphp2vIwcG41iPH168B1T7fFZ12jEBACwLbb/+iJmZw2XP
    MWY0st8vAgAvkqbfdM0GwwTb1cSTQ+is7fX+evgLT91bjloPXjTKTSK58CEQH7Bj2nf64Qt1Gjcz
    gjICAuPEZcKXPZs39gYA9v12Tdj61HsInTQaRRK9gIzeYEnO4+8oOXN9cZqMQl4ez4AunOJ7oAN3
    1MXbAwC01EzxJ+s4V5xBy0gWCJEjeasBIJOG2PoomINJhmeOlyKzpIoNEoE5OtYehaG1ysbrWLhP
    L+uw/GT37tEmwIsW6pVRZYI9hlaWlax56S5NN6sAK6lW1WoQIOTLhlItCLRHzxZieOPIGPLd+H5I
    LAMiTQDwBgQKDKL58H3MBHeWjxmzGeZmgBiZkRMAAPHaHOiOV0gR4924DgPfEmkAnNCX37XVp2M3
    UFbiVjlumbrvagTXBGgJ0zYUf1m1+kci+a1MqTqcyJEznDlqrBNXWXF1r9N0ogHs5CA5k9XM/1EJ
    6eShFlCrIbne6/jdYeqMNqje0wbA90Ai2WxhCkrjGLt7DkmuqHkWMN89ANDx27P9lrBqgV/AMGtW
    Ur1gWXXMCDjfPQBGNQyW0B5W7kFgqbIEWxUP8EARgSNbnn73ABD6ZFY9o8CQvscLZ8/SrDZhl40a
    FLffOwD0dq1R2bsNT+uoaJdpmdRX4wPMknHXAA6hAAbNoL2wtQVAtVFExARWWmc1ih1v1wCGUPf4
    VCegXgYFacBkzBlJYzcGUDEPlQjirgGO9JRu/nI6QeIbYJkI5ts6xK6j12FcdcwdAAVC8aidLDmG
    ZBibJq5EBlczuALI3QQYur/a7vkjk4SXWQQEVTXLmILMeeuAhAXBrgEcQrILYXAc18mx6L2BWaZV
    NAvL7NG5HQAFUNjcANLPTKq4IuEdRnZzETsASLI9gnTGUM7rsnfW9ncBUfUndgAsoM27jw4JQNja
    SQYMK4DDmI8dAIsAgDzBKF28WgN04gs7ACZIxwNGlUPvj36B7SdQYbxnQjKzUjEFdqwdACSJx5+1
    yEE1sgAFewhk3j6zGlgVTfRoBwBJsPGZGdBFqUxZOQOQivZgTMoOgAahLjJqQmUrpD1/oLv+Z7TH
    DoAzEt5MRoLIazdjG1hc3m0jil7ewAvisAGhKrN3ACwgNLvQ7098VOXj+trqe5Weyl/JbG+sHQAF
    gocPJkUp46heoBr3H92bjcP4DzsAigCw28agnY0uGhn1EWZteVf1j5aQIzO0A6BIKB/zmIDCUt3F
    ZGT3LTPZzF1HW+wmYDHhFTGvj4AwX+8cmk0+G9xZlTPwxtsB0CCkie0bUzpcLMc/XeTNJVeeY0G3
    A+BM5OULkCBiOo8w5yr37E7gC9OowSV6LWbZQsarZ6U9Oq+/ZwfAQkKo2Nvj6PLu3aaNbdaIa4X6
    303AC9Oo3Yx9xUyu9SqJMmZVTAQLnh0AiwiBnwwA6Dgi9yBXwNjvrv3PNMUOgIU06q9kScyBAEEA
    cH+sHfCcxEqeoKsVdgAsIjS/tkUi0buGN8feBOIroPdA10HsaoUdAIsINj2TfhBeP5N7UUlU6cre
    pR0AZyI012K3rfE6mqGopCvZHe2wA2ARwQkc7VDiaQBvDGZrGnt+JuG0A2ARoV+AZwY+mi10kDTy
    xrDJppn6gCgRtQPgDISuIXDyRtIvgPB6LOpdVEaZQjahFKWB90jgGUk3pRZtMDIJ7+9OTYCuI/DU
    fDcoFJmHHQALCczWk3x/5ZeT2x3abGSwq/YjR3E3AWcmqH1v8m0PZqsBdB1BxvyVBSQ7ABaSSHW0
    84jtR4j77PsDM6qejQjuJuBMhBb6EVNgKuT60bo/8+AZLbCXhL0SAEYSrSc/2wQrW9tXJX03AS9M
    CO2iyzra0GF3VLlGtICtIewUelQAsmuAFybx7q+dTuu2egiaA+VjUTDIWxp2HcUdAK9EXv0gKoiw
    JPw5qDCOtAPrRMqx2x0AbwcANlmE6/TG3ZW8PwOIXQO8Enk7oHlvHeuAEdN+JoojeOd2ALwSVWoH
    5Nqbi997FepNMyu5gt0HeCOE3c5YAIDuj9pAtMf1YBf2TCPY/3cf4BVo1JGc0QQ6izjaNTVaNu4m
    4A0QeglVASCkm1CgBoGNE+xFoW+ERIqZbWpGhC16AQDUF0bqP8wG/unx5p1ejt79+d3DR9lq96pH
    f300BX8//PvprzDwv4e/PFwevv2v6fZIP6q/lv71SP8Htsucd3ybM8MAAAAASUVORK5CYII=
