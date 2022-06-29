// Copyright 2020 Mathias Bynens. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
author: Mathias Bynens
esid: sec-names-and-keywords
description: |
  Test that Unicode v12.0.0 ID_Start characters are accepted as
  identifier start characters in escaped form, i.e.
  - \uXXXX or \u{XXXX} for BMP symbols
  - \u{XXXXXX} for astral symbols
info: |
  Generated by https://github.com/mathiasbynens/caniunicode
---*/
var _ຆຉຌຎຏຐຑຒຓຘຠຨຩຬᳲᳳᳺꞺꞻꞼꞽꞾꞿꟂꟃꟄꟅꟆꭦꭧ_ud803_udfe0_ud803_udfe1_ud803_udfe2_ud803_udfe3_ud803_udfe4_ud803_udfe5_ud803_udfe6_ud803_udfe7_ud803_udfe8_ud803_udfe9_ud803_udfea_ud803_udfeb_ud803_udfec_ud803_udfed_ud803_udfee_ud803_udfef_ud803_udff0_ud803_udff1_ud803_udff2_ud803_udff3_ud803_udff4_ud803_udff5_ud803_udff6_ud805_udc5f_ud805_udeb8_ud806_udda0_ud806_udda1_ud806_udda2_ud806_udda3_ud806_udda4_ud806_udda5_ud806_udda6_ud806_udda7_ud806_uddaa_ud806_uddab_ud806_uddac_ud806_uddad_ud806_uddae_ud806_uddaf_ud806_uddb0_ud806_uddb1_ud806_uddb2_ud806_uddb3_ud806_uddb4_ud806_uddb5_ud806_uddb6_ud806_uddb7_ud806_uddb8_ud806_uddb9_ud806_uddba_ud806_uddbb_ud806_uddbc_ud806_uddbd_ud806_uddbe_ud806_uddbf_ud806_uddc0_ud806_uddc1_ud806_uddc2_ud806_uddc3_ud806_uddc4_ud806_uddc5_ud806_uddc6_ud806_uddc7_ud806_uddc8_ud806_uddc9_ud806_uddca_ud806_uddcb_ud806_uddcc_ud806_uddcd_ud806_uddce_ud806_uddcf_ud806_uddd0_ud806_udde1_ud806_udde3_ud806_ude84_ud806_ude85_ud81b_udf45_ud81b_udf46_ud81b_udf47_ud81b_udf48_ud81b_udf49_ud81b_udf4a_ud81b_udfe3_ud821_udff2_ud821_udff3_ud821_udff4_ud821_udff5_ud821_udff6_ud821_udff7_ud82c_udd50_ud82c_udd51_ud82c_udd52_ud82c_udd64_ud82c_udd65_ud82c_udd66_ud82c_udd67_ud838_udd00_ud838_udd01_ud838_udd02_ud838_udd03_ud838_udd04_ud838_udd05_ud838_udd06_ud838_udd07_ud838_udd08_ud838_udd09_ud838_udd0a_ud838_udd0b_ud838_udd0c_ud838_udd0d_ud838_udd0e_ud838_udd0f_ud838_udd10_ud838_udd11_ud838_udd12_ud838_udd13_ud838_udd14_ud838_udd15_ud838_udd16_ud838_udd17_ud838_udd18_ud838_udd19_ud838_udd1a_ud838_udd1b_ud838_udd1c_ud838_udd1d_ud838_udd1e_ud838_udd1f_ud838_udd20_ud838_udd21_ud838_udd22_ud838_udd23_ud838_udd24_ud838_udd25_ud838_udd26_ud838_udd27_ud838_udd28_ud838_udd29_ud838_udd2a_ud838_udd2b_ud838_udd2c_ud838_udd37_ud838_udd38_ud838_udd39_ud838_udd3a_ud838_udd3b_ud838_udd3c_ud838_udd3d_ud838_udd4e_ud838_udec0_ud838_udec1_ud838_udec2_ud838_udec3_ud838_udec4_ud838_udec5_ud838_udec6_ud838_udec7_ud838_udec8_ud838_udec9_ud838_udeca_ud838_udecb_ud838_udecc_ud838_udecd_ud838_udece_ud838_udecf_ud838_uded0_ud838_uded1_ud838_uded2_ud838_uded3_ud838_uded4_ud838_uded5_ud838_uded6_ud838_uded7_ud838_uded8_ud838_uded9_ud838_udeda_ud838_udedb_ud838_udedc_ud838_udedd_ud838_udede_ud838_udedf_ud838_udee0_ud838_udee1_ud838_udee2_ud838_udee3_ud838_udee4_ud838_udee5_ud838_udee6_ud838_udee7_ud838_udee8_ud838_udee9_ud838_udeea_ud838_udeeb_ud83a_udd4b;