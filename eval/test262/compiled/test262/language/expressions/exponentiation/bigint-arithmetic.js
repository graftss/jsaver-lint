// Copyright (C) 2017 Robin Templeton. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-exp-operator-runtime-semantics-evaluation
description: BigInt exponentiation arithmetic
features: [BigInt]
---*/
assert.sameValue(Math.pow(0x123n, 0x123n), 0x37AA7FAA38F2F6026AABEFE979EA730BA9EA4CB99E2E3F645D515D3BBE2D84BCD89F8A034BADF3E3DC0CF417258371B31F4555DC0883DA96760AB157DB7DFFF5E3E97A3EAAB8328B2B178060B5A5E4C4DD8BC8D66B7F4D9F0E0B1AC3A566FDE0A15EBF8DBDBD0565C5FBDB7C123CF250E271DF5C38BC6746A1327F09C7FB4B96E0EDA45C429799CA80B1DB039692C70DFFE4E66F1D9CAB4270863B09A7918F774D686F685F560FEDC6B7B85CB45DE5EEEF6A5FE2FC8B5037FB421204641909347C91F2DC252F49B8F310E867E56D1CA2E81EE9A3AA568682C7B8B41D709A2E7F8D9A8D8C56D6BE78B6CA8365E362B81A64974C315FB8FA50CED4F7944F28FA3ECA77B8BCB56DC69814328F891E6065D108EEE7B8E695038090CCA10C0E68DD7A36CFAA1C26CDFEC369FBn, 'The result of (0x123n ** 0x123n) is 0x37AA7FAA38F2F6026AABEFE979EA730BA9EA4CB99E2E3F645D515D3BBE2D84BCD89F8A034BADF3E3DC0CF417258371B31F4555DC0883DA96760AB157DB7DFFF5E3E97A3EAAB8328B2B178060B5A5E4C4DD8BC8D66B7F4D9F0E0B1AC3A566FDE0A15EBF8DBDBD0565C5FBDB7C123CF250E271DF5C38BC6746A1327F09C7FB4B96E0EDA45C429799CA80B1DB039692C70DFFE4E66F1D9CAB4270863B09A7918F774D686F685F560FEDC6B7B85CB45DE5EEEF6A5FE2FC8B5037FB421204641909347C91F2DC252F49B8F310E867E56D1CA2E81EE9A3AA568682C7B8B41D709A2E7F8D9A8D8C56D6BE78B6CA8365E362B81A64974C315FB8FA50CED4F7944F28FA3ECA77B8BCB56DC69814328F891E6065D108EEE7B8E695038090CCA10C0E68DD7A36CFAA1C26CDFEC369FBn');
assert.sameValue(Math.pow(0x123n, 0xFFn), 0x8D5BB75861377EC967BF78FDF39CE51696FBD34722999943F8865938772B517167CD5ED775A78987F5106831F4978E0709032B26ED8F13F814699DB8AB3ACD5CF631F2D8B8B706FCF5EF441AAEE745A795EC5CB86A5E8D87D09F648EFC557B98F73E750FEC9AED061D47806F269CCCDFB6D513912A82AE79B171D76AF6D926BC4F4C4DA43A6EFB4D9D1672E356CC1F74A29AF80D53A8F27592F6191AB9B3D57FA2C435CB2CE8F18A3B3448F88F4BAD3606A9878DA9528B569BADAC0C1EC0B1A2B06CD4C64DEEC940807DFD05C56E3E17ADB1A88EDAF0D67C87C1F871BFB5C47CAE8365FE33538317EE2DF4EE52636CE1BDA9E41C7DA72826E4C097A53BD73D8D697E10D28Bn, 'The result of (0x123n ** 0xFFn) is 0x8D5BB75861377EC967BF78FDF39CE51696FBD34722999943F8865938772B517167CD5ED775A78987F5106831F4978E0709032B26ED8F13F814699DB8AB3ACD5CF631F2D8B8B706FCF5EF441AAEE745A795EC5CB86A5E8D87D09F648EFC557B98F73E750FEC9AED061D47806F269CCCDFB6D513912A82AE79B171D76AF6D926BC4F4C4DA43A6EFB4D9D1672E356CC1F74A29AF80D53A8F27592F6191AB9B3D57FA2C435CB2CE8F18A3B3448F88F4BAD3606A9878DA9528B569BADAC0C1EC0B1A2B06CD4C64DEEC940807DFD05C56E3E17ADB1A88EDAF0D67C87C1F871BFB5C47CAE8365FE33538317EE2DF4EE52636CE1BDA9E41C7DA72826E4C097A53BD73D8D697E10D28Bn');
assert.sameValue(Math.pow(0x123n, 0x3n), 0x178027Bn, 'The result of (0x123n ** 0x3n) is 0x178027Bn');
assert.sameValue(Math.pow(0x123n, 0x2n), 0x14AC9n, 'The result of (0x123n ** 0x2n) is 0x14AC9n');
assert.sameValue(Math.pow(0x123n, 0x1n), 0x123n, 'The result of (0x123n ** 0x1n) is 0x123n');
assert.sameValue(Math.pow(0xFFn, 0x123n), 0x51F5CA2E1A36F5FF1ED3D393D76FBC3612B38EB64E00EDAC5E95ADE0D16D0B044C8E9F2B77B3F31AF9159F482205541E9D3BE9D248FF39CE6524874EBCA60E06302E8B505D11EEEEE869C7F801A82B9739C197E6D63A1EB2D29B5AD5EED4773C762106E9F66BFCB6C11450218973C69DED3FE51FF881AD0430675BF54320513EA766117C50C554E86E22A5ACFD8047D5470B4FCBCB9EFC86196CA77C58F1BEB09F76160D641B82E2481BEDAE089207D49FE0FB7DE14B6C4BC82E9C58140746AC8E74C3353AAF5F9CF47ED1F87C52F463C053DB63CD08CC9866EBA274D39B6B357ADADAD4D210167EF7363453D42BC225D90070336861F2D259489D78B7F04B05FE65E29151ADD2B8F4D318011988550CE590DBA4C868AC65AA325051DF613D6C2E22FFn, 'The result of (0xFFn ** 0x123n) is 0x51F5CA2E1A36F5FF1ED3D393D76FBC3612B38EB64E00EDAC5E95ADE0D16D0B044C8E9F2B77B3F31AF9159F482205541E9D3BE9D248FF39CE6524874EBCA60E06302E8B505D11EEEEE869C7F801A82B9739C197E6D63A1EB2D29B5AD5EED4773C762106E9F66BFCB6C11450218973C69DED3FE51FF881AD0430675BF54320513EA766117C50C554E86E22A5ACFD8047D5470B4FCBCB9EFC86196CA77C58F1BEB09F76160D641B82E2481BEDAE089207D49FE0FB7DE14B6C4BC82E9C58140746AC8E74C3353AAF5F9CF47ED1F87C52F463C053DB63CD08CC9866EBA274D39B6B357ADADAD4D210167EF7363453D42BC225D90070336861F2D259489D78B7F04B05FE65E29151ADD2B8F4D318011988550CE590DBA4C868AC65AA325051DF613D6C2E22FFn');
assert.sameValue(Math.pow(0xFFn, 0xFFn), 0x5E5C8B0EB95AB08F9D37EF127FC01BD0E33DE52647528396D78D5F8DA31989E67814F6BBA1FB0F0207010FF5F2347B19D5F6598FC91BF5A88F77DAA3D7B382FEC484F3D205C06A34445384C0E7AB0D883788C68C012CB433055EDDA746A48409444EA91147273B79FC3EABB70ECA552AF650C234BB01ED404427F17CDDDD71D08E39EF9C3982E3CE44E670456AA8154C1FDBD9C35947F494636A425C69BF89E9C75AD3B7A0A559AF0F5DA9947C8DEBA64417310713B23E7EF4DE50BB2A3E90BC2AC3DA5201CCA8D6E5DFEA887C4F7A4E92175D9F88BD2779B57F9EB35BE7528F965A06DA0AC41DCB3A34F1D8AB7D8FEE620A94FAA42C395997756B007FFEFFn, 'The result of (0xFFn ** 0xFFn) is 0x5E5C8B0EB95AB08F9D37EF127FC01BD0E33DE52647528396D78D5F8DA31989E67814F6BBA1FB0F0207010FF5F2347B19D5F6598FC91BF5A88F77DAA3D7B382FEC484F3D205C06A34445384C0E7AB0D883788C68C012CB433055EDDA746A48409444EA91147273B79FC3EABB70ECA552AF650C234BB01ED404427F17CDDDD71D08E39EF9C3982E3CE44E670456AA8154C1FDBD9C35947F494636A425C69BF89E9C75AD3B7A0A559AF0F5DA9947C8DEBA64417310713B23E7EF4DE50BB2A3E90BC2AC3DA5201CCA8D6E5DFEA887C4F7A4E92175D9F88BD2779B57F9EB35BE7528F965A06DA0AC41DCB3A34F1D8AB7D8FEE620A94FAA42C395997756B007FFEFFn');
assert.sameValue(Math.pow(0xFFn, 0x3n), 0xFD02FFn, 'The result of (0xFFn ** 0x3n) is 0xFD02FFn');
assert.sameValue(Math.pow(0xFFn, 0x2n), 0xFE01n, 'The result of (0xFFn ** 0x2n) is 0xFE01n');
assert.sameValue(Math.pow(0xFFn, 0x1n), 0xFFn, 'The result of (0xFFn ** 0x1n) is 0xFFn');
assert.sameValue(Math.pow(0x3n, 0x123n), 0x25609213623D7D6219085CF49D306450BF6519835586C19D3A4F3A2C5F35B44A300C8A76E11708B5495B9C3EE756BBF19E3FD15CE625D3C0539Bn, 'The result of (0x3n ** 0x123n) is 0x25609213623D7D6219085CF49D306450BF6519835586C19D3A4F3A2C5F35B44A300C8A76E11708B5495B9C3EE756BBF19E3FD15CE625D3C0539Bn');
assert.sameValue(Math.pow(0x3n, 0xFFn), 0x11F1B08E87EC42C5D83C3218FC83C41DCFD9F4428F4F92AF1AAA80AA46162B1F71E981273601F4AD1DD4709B5ACA650265A6ABn, 'The result of (0x3n ** 0xFFn) is 0x11F1B08E87EC42C5D83C3218FC83C41DCFD9F4428F4F92AF1AAA80AA46162B1F71E981273601F4AD1DD4709B5ACA650265A6ABn');
assert.sameValue(Math.pow(0x3n, 0x3n), 0x1Bn, 'The result of (0x3n ** 0x3n) is 0x1Bn');
assert.sameValue(Math.pow(0x3n, 0x2n), 0x9n, 'The result of (0x3n ** 0x2n) is 0x9n');
assert.sameValue(Math.pow(0x3n, 0x1n), 0x3n, 'The result of (0x3n ** 0x1n) is 0x3n');
assert.sameValue(Math.pow(0x2n, 0x123n), 0x8000000000000000000000000000000000000000000000000000000000000000000000000n, 'The result of (0x2n ** 0x123n) is 0x8000000000000000000000000000000000000000000000000000000000000000000000000n');
assert.sameValue(Math.pow(0x2n, 0xFFn), 0x8000000000000000000000000000000000000000000000000000000000000000n, 'The result of (0x2n ** 0xFFn) is 0x8000000000000000000000000000000000000000000000000000000000000000n');
assert.sameValue(Math.pow(0x2n, 0x3n), 0x8n, 'The result of (0x2n ** 0x3n) is 0x8n');
assert.sameValue(Math.pow(0x2n, 0x2n), 0x4n, 'The result of (0x2n ** 0x2n) is 0x4n');
assert.sameValue(Math.pow(0x2n, 0x1n), 0x2n, 'The result of (0x2n ** 0x1n) is 0x2n');
assert.sameValue(Math.pow(0x1n, 0x123n), 0x1n, 'The result of (0x1n ** 0x123n) is 0x1n');
assert.sameValue(Math.pow(0x1n, 0xFFn), 0x1n, 'The result of (0x1n ** 0xFFn) is 0x1n');
assert.sameValue(Math.pow(0x1n, 0x3n), 0x1n, 'The result of (0x1n ** 0x3n) is 0x1n');
assert.sameValue(Math.pow(0x1n, 0x2n), 0x1n, 'The result of (0x1n ** 0x2n) is 0x1n');
assert.sameValue(Math.pow(0x1n, 0x1n), 0x1n, 'The result of (0x1n ** 0x1n) is 0x1n');