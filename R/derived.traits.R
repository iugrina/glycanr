igg.derived.glycans <- function(data) {
    
    # derived glycans
    data$`FGS/(FG+FGS)` = with(data, (GP16 + GP18 + GP23)/(GP16 + GP18 + GP23 + GP8 + 
        GP9 + GP14)) * 100
    data$`FBGS/(FBG+FBGS)` = with(data, (GP19 + GP24)/(GP19 + GP24 + GP10 + GP11 + 
        GP15)) * 100
    data$`FGS/(F+FG+FGS)` = with(data, (GP16 + GP18 + GP23)/(GP16 + GP18 + GP23 + 
        GP4 + GP8 + GP9 + GP14)) * 100
    data$`FBGS/(FB+FBG+FBGS)` = with(data, (GP19 + GP24)/(GP19 + GP24 + GP6 + GP10 + 
        GP11 + GP15)) * 100
    data$`FG1S1/(FG1+FG1S1)` = with(data, GP16/(GP16 + GP8 + GP9)) * 100
    data$`FG2S1/(FG2+FG2S1+FG2S2)` = with(data, GP18/(GP18 + GP14 + GP23)) * 100
    data$`FG2S2/(FG2+FG2S1+FG2S2)` = with(data, GP23/(GP23 + GP14 + GP18)) * 100
    data$`FBG2S1/(FBG2+FBG2S1+FBG2S2)` = with(data, GP19/(GP19 + GP15 + GP24)) * 
        100
    data$`FBG2S2/(FBG2+FBG2S1+FBG2S2)` = with(data, GP24/(GP24 + GP15 + GP19)) * 
        100
    data$`FtotalS1/FtotalS2` = with(data, (GP16 + GP18 + GP19)/(GP23 + GP24))
    data$`FS1/FS2` = with(data, (GP16 + GP18)/GP23)
    data$`FBS1/FBS2` = with(data, GP19/GP24)
    data$`FBStotal/FStotal` = with(data, (GP19 + GP24)/(GP16 + GP18 + GP23))
    data$`FBS1/FS1` = with(data, GP19/(GP16 + GP18))
    data$`FBS1/(FS1+FBS1)` = with(data, GP19/(GP16 + GP18 + GP19)) * 100
    data$`FBS2/FS2` = with(data, GP24/GP23)
    data$`FBS2/(FS2+FBS2)` = with(data, GP24/(GP23 + GP24)) * 100
    data$`S total` = with(data, GP16 + GP17 + GP18 + GP19 + GP20 + GP21 + GP22 + 
        GP23 + GP24)
    data$`S1 total` = with(data, GP16 + GP17 + GP18 + GP19)
    data$`S2 total` = with(data, GP21 + GP22 + GP23 + GP24)
    
    # neutral glycans
    GPn = with(data, GP1 + GP2 + GP4 + GP6 + GP7 + GP8 + GP9 + GP10 + GP11 + GP12 + 
        GP13 + GP14 + GP15)
    data$GP1n = with(data, GP1/GPn) * 100
    data$GP2n = with(data, GP2/GPn) * 100
    data$GP4n = with(data, GP4/GPn) * 100
    data$GP5n = with(data, GP5/GPn) * 100
    data$GP6n = with(data, GP6/GPn) * 100
    data$GP7n = with(data, GP7/GPn) * 100
    data$GP8n = with(data, GP8/GPn) * 100
    data$GP9n = with(data, GP9/GPn) * 100
    data$GP10n = with(data, GP10/GPn) * 100
    data$GP11n = with(data, GP11/GPn) * 100
    data$GP12n = with(data, GP12/GPn) * 100
    data$GP13n = with(data, GP13/GPn) * 100
    data$GP14n = with(data, GP14/GPn) * 100
    data$GP15n = with(data, GP15/GPn) * 100
    
    # neutral derived glycans
    data$G0n = with(data, (GP1n + GP2n + GP4n + GP6n))
    data$G1n = with(data, (GP7n + GP8n + GP9n + GP10n + GP11n))
    data$G2n = with(data, (GP12n + GP13n + GP14n + GP15n))
    data$`Fn total` = with(data, (GP1n + GP4n + GP6n + GP8n + GP9n + GP10n + GP11n + 
        GP14n + GP15n))
    data$`FG0n total/G0n` = with(data, (GP1n + GP4n + GP6n)/G0n) * 100
    data$`FG1n total/G1n` = with(data, (GP8n + GP9n + GP10n + GP11n)/G1n) * 100
    data$`FG2n total /G2n` = with(data, (GP14n + GP15)/G2n) * 100
    data$Fn = with(data, (GP1n + GP4n + GP8n + GP9n + GP14n))
    data$`FG0n/G0n` = with(data, (GP1n + GP4n)/G0n) * 100
    data$`FG1n/G1n` = with(data, (GP8n + GP9n)/G1n) * 100
    data$`FG2n/G2n` = with(data, GP14n/G2n) * 100
    data$FBn = with(data, (GP6n + GP10n + GP11n + GP15n))
    data$`FBG0n/G0n` = with(data, GP6n/G0n) * 100
    data$`FBG1n/G1n` = with(data, (GP10n + GP11n)/G1n) * 100
    data$`FBG2n/G2n` = with(data, GP15n/G2n) * 100
    data$`FBn/Fn` = with(data, FBn/Fn) * 100
    data$`FBn/Fn total` = with(data, FBn/`Fn total`) * 100
    data$`Fn/(Bn + FBn)` = with(data, Fn/(GP13n + FBn))
    data$`Bn/(Fn + FBn) ` = with(data, GP13n/(Fn + FBn)) * 1000
    data$`FBG2n/FG2n` = with(data, GP15n/GP14n)
    data$`FBG2n /(FG2n + FBG2n )` = with(data, GP15n/(GP14n + GP15n)) * 100
    data$`FG2n/(BG2n + FBG2n)` = with(data, GP14n/(GP13n + GP15n))
    data$`BG2n/(FG2n + FBG2n) ` = with(data, GP13n/(GP14n + GP15n)) * 1000
    
    
    return(data)
}
 
