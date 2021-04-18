/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check;


/**
 * List of HTML characters.
 */
public enum HtmlCharacters {

  SYMBOL_SPACE(32, null, ' ', false, false),
  SYMBOL_EXCLAMATION_MARK(33, null, '!'),
  SYMBOL_QUOTATION_MARK(34, "quot", '\"'),
  SYMBOL_NUMBER_SIGN(35, null, '#'),
  SYMBOL_DOLLAR_SIGN(36, null, '$'),
  SYMBOL_PERCENT_SIGN(37, null, '%'),
  SYMBOL_AMPERSAND(38, "amp", '&', false, false),
  SYMBOL_APOSTROPHE(39, "apos", '\'', false, false),
  SYMBOL_LEFT_PARENTHESIS(40, null, '('),
  SYMBOL_RIGHT_PARENTHESIS(41, null, ')'),
  SYMBOL_ASTERISK(42, null, '*'),
  SYMBOL_PLUS_SIGN(43, null, '+'),
  SYMBOL_COMMA(44, null, ','),
  SYMBOL_HYPHEN(45, null, '-'),
  SYMBOL_PERIOD(46, null, '.'),
  SYMBOL_SLASH(47, null, '/'),
  DIGIT_0(48, null, '0'),
  DIGIT_1(49, null, '1'),
  DIGIT_2(50, null, '2'),
  DIGIT_3(51, null, '3'),
  DIGIT_4(52, null, '4'),
  DIGIT_5(53, null, '5'),
  DIGIT_6(54, null, '6'),
  DIGIT_7(55, null, '7'),
  DIGIT_8(56, null, '8'),
  DIGIT_9(57, null, '9'),
  SYMBOL_COLON(58, null, ':', false, false),
  SYMBOL_SEMICOLON(59, null, ';', false, false),
  SYMBOL_LESS_THAN(60, "lt", '<', false, false),
  SYMBOL_EQUAL(61, null, '=', false, false),
  SYMBOL_GREATER_THAN(62, "gt", '>', false, false),
  SYMBOL_QUESTION_MARK(63, null, '?'),
  SYMBOL_AT_SIGN(64, null, '@'),
  LETTER_CAPITAL_A(65, null, 'A'),
  LETTER_CAPITAL_B(66, null, 'B'),
  LETTER_CAPITAL_C(67, null, 'C'),
  LETTER_CAPITAL_D(68, null, 'D'),
  LETTER_CAPITAL_E(69, null, 'E'),
  LETTER_CAPITAL_F(70, null, 'F'),
  LETTER_CAPITAL_G(71, null, 'G'),
  LETTER_CAPITAL_H(72, null, 'H'),
  LETTER_CAPITAL_I(73, null, 'I'),
  LETTER_CAPITAL_J(74, null, 'J'),
  LETTER_CAPITAL_K(75, null, 'K'),
  LETTER_CAPITAL_L(76, null, 'L'),
  LETTER_CAPITAL_M(77, null, 'M'),
  LETTER_CAPITAL_N(78, null, 'N'),
  LETTER_CAPITAL_O(79, null, 'O'),
  LETTER_CAPITAL_P(80, null, 'P'),
  LETTER_CAPITAL_Q(81, null, 'Q'),
  LETTER_CAPITAL_R(82, null, 'R'),
  LETTER_CAPITAL_S(83, null, 'S'),
  LETTER_CAPITAL_T(84, null, 'T'),
  LETTER_CAPITAL_U(85, null, 'U'),
  LETTER_CAPITAL_V(86, null, 'V'),
  LETTER_CAPITAL_W(87, null, 'W'),
  LETTER_CAPITAL_X(88, null, 'X'),
  LETTER_CAPITAL_Y(89, null, 'Y'),
  LETTER_CAPITAL_Z(90, null, 'Z'),
  SYMBOL_LEFT_SQUARE_BRACKET(91, null, '[', false, false),
  SYMBOL_BACKSLASH(92, null, '\\'),
  SYMBOL_RIGHT_SQUARE_BRACKET(93, null, ']', false, false),
  SYMBOL_CARET(94, null, '^'),
  SYMBOL_UNDERSCORE(95, null, '_'),
  SYMBOL_GRAVE_ACCENT(96, null, '`'),
  LETTER_SMALL_A(97, null, 'a'),
  LETTER_SMALL_B(98, null, 'b'),
  LETTER_SMALL_C(99, null, 'c'),
  LETTER_SMALL_D(100, null, 'd'),
  LETTER_SMALL_E(101, null, 'e'),
  LETTER_SMALL_F(102, null, 'f'),
  LETTER_SMALL_G(103, null, 'g'),
  LETTER_SMALL_H(104, null, 'h'),
  LETTER_SMALL_I(105, null, 'i'),
  LETTER_SMALL_J(106, null, 'j'),
  LETTER_SMALL_K(107, null, 'k'),
  LETTER_SMALL_L(108, null, 'l'),
  LETTER_SMALL_M(109, null, 'm'),
  LETTER_SMALL_N(110, null, 'n'),
  LETTER_SMALL_O(111, null, 'o'),
  LETTER_SMALL_P(112, null, 'p'),
  LETTER_SMALL_Q(113, null, 'q'),
  LETTER_SMALL_R(114, null, 'r'),
  LETTER_SMALL_S(115, null, 's'),
  LETTER_SMALL_T(116, null, 't'),
  LETTER_SMALL_U(117, null, 'u'),
  LETTER_SMALL_V(118, null, 'v'),
  LETTER_SMALL_W(119, null, 'w'),
  LETTER_SMALL_X(120, null, 'x'),
  LETTER_SMALL_Y(121, null, 'y'),
  LETTER_SMALL_Z(122, null, 'z'),
  SYMBOL_LEFT_CURLY_BRACE(123, null, '{', false, false),
  SYMBOL_VERTICAL_BAR(124, null, '|', false, false),
  SYMBOL_RIGHT_CURLY_BRACE(125, null, '}', false, false),
  SYMBOL_TILDE(126, null, '~', false, false),
  SYMBOL_NON_BREAKING_SPACE(160, "nbsp", '\u00A0', false, false),
  SYMBOL_INVERTED_EXCLAMATION_MARK(161, "iexcl", '¡'),
  SYMBOL_CENT(162, "cent", '¢'),
  SYMBOL_POUND(163, "pound", '£'),
  SYMBOL_CURRENCY(164, "curren", '¤'),
  SYMBOL_YEN(165, "yen", '¥'),
  SYMBOL_BROKEN_VERTICAL_BAR(166, "brvbar", '¦'),
  SYMBOL_SECTION(167, "sect", '§'),
  SYMBOL_SPACING_DIAERESIS(168, "uml", '¨'),
  SYMBOL_COPYRIGHT(169, "copy", '©'),
  SYMBOL_FEMININE_ORDINAL_INDICATOR(170, "ordf", 'ª'),
  SYMBOL_LEFT_ANGLE_QUOTATION_MARK(171, "laquo", '«'),
  SYMBOL_NEGATION(172, "not", '¬'),
  SYMBOL_SOFT_HYPHEN(173, "shy", '\u00AD'),
  SYMBOL_REGISTERED_TRADEMARK(174, "reg", '®'),
  SYMBOL_SPACING_MACRON(175, "macr", '¯'),
  SYMBOL_DEGREE(176, "deg", '°'),
  SYMBOL_PLUS_OR_MINUS(177, "plusmn", '±'),
  SYMBOL_SUPERSCRIPT_2(178, "sup2", '²'),
  SYMBOL_SUPERSCRIPT_3(179, "sup3", '³'),
  SYMBOL_SPACING_ACUTE(180, "acute", '´'),
  SYMBOL_MICRO(181, "micro", 'µ'),
  SYMBOL_PARAGRAPH(182, "para", '¶'),
  SYMBOL_MIDDLE_DOT(183, "middot", '·'),
  SYMBOL_SPACING_CEDILLA(184, "cedil", '¸'),
  SYMBOL_SUPERSCRIPT_1(185, "sup1", '¹'),
  SYMBOL_MASCULINE_ORDINAL_INDICATOR(186, "ordm", 'º'),
  SYMBOL_RIGHT_ANGLE_QUOTATION_MARK(187, "raquo", '»'),
  SYMBOL_FRACTION_1_4(188, "frac14", '¼'),
  SYMBOL_FRACTION_1_2(189, "frac12", '½'),
  SYMBOL_FRACTION_3_4(190, "frac34", '¾'),
  SYMBOL_INVERTED_QUESTION_MARK(191, "iquest", '¿'),
  LETTER_CAPITAL_A_GRAVE_ACCENT(192, "Agrave", 'À'),
  LETTER_CAPITAL_A_ACUTE_ACCENT(193, "Aacute", 'Á'),
  LETTER_CAPITAL_A_CIRCUMFLEX_ACCENT(194, "Acirc", 'Â'),
  LETTER_CAPITAL_A_TILDE(195, "Atilde", 'Ã'),
  LETTER_CAPITAL_A_UMLAUT_MARK(196, "Auml", 'Ä'),
  LETTER_CAPITAL_A_RING(197, "Aring", 'Å'),
  LETTER_CAPITAL_AE(198, "AElig", 'Æ'),
  LETTER_CAPITAL_C_CEDILLA(199, "Ccedil", 'Ç'),
  LETTER_CAPITAL_E_GRAVE_ACCENT(200, "Egrave", 'È'),
  LETTER_CAPITAL_E_ACUTE_ACCENT(201, "Eacute", 'É'),
  LETTER_CAPITAL_E_CIRCUMFLEX_ACCENT(202, "Ecirc", 'Ê'),
  LETTER_CAPITAL_E_UMLAUT_MARK(203, "Euml", 'Ë'),
  LETTER_CAPITAL_I_GRAVE_ACCENT(204, "Igrave", 'Ì'),
  LETTER_CAPITAL_I_ACUTE_ACCENT(205, "Iacute", 'Í'),
  LETTER_CAPITAL_I_CIRCUMFLEX_ACCENT(206, "Icirc", 'Î'),
  LETTER_CAPITAL_I_UMLAUT_MARK(207, "Iuml", 'Ï'),
  LETTER_CAPITAL_ETH(208, "ETH", 'Ð'),
  LETTER_CAPITAL_N_TILDE(209, "Ntilde", 'Ñ'),
  LETTER_CAPITAL_O_GRAVE_ACCENT(210, "Ograve", 'Ò'),
  LETTER_CAPITAL_O_ACUTE_ACCENT(211, "Oacute", 'Ó'),
  LETTER_CAPITAL_O_CIRCUMFLEX_ACCENT(212, "Ocirc", 'Ô'),
  LETTER_CAPITAL_O_TILDE(213, "Otilde", 'Õ'),
  LETTER_CAPITAL_O_UMLAUT_MARK(214, "Ouml", 'Ö'),
  SYMBOL_MULTIPLICATION(215, "times", '×'),
  LETTER_CAPITAL_O_SLASH(216, "Oslash", 'Ø'),
  LETTER_CAPITAL_U_GRAVE_ACCENT(217, "Ugrave", 'Ù'),
  LETTER_CAPITAL_U_ACUTE_ACCENT(218, "Uacute", 'Ú'),
  LETTER_CAPITAL_U_CIRCUMFLEX_ACCENT(219, "Ucirc", 'Û'),
  LETTER_CAPITAL_U_UMLAUT_MARK(220, "Uuml", 'Ü'),
  LETTER_CAPITAL_Y_ACUTE_ACCENT(221, "Yacute", 'Ý'),
  LETTER_CAPITAL_THORN(222, "THORN", 'Þ'),
  LETTER_SMALL_SHARP_S(223, "szlig", 'ß'),
  LETTER_SMALL_A_GRAVE_ACCENT(224, "agrave", 'à'),
  LETTER_SMALL_A_ACUTE_ACCENT(225, "aacute", 'á'),
  LETTER_SMALL_A_CIRCUMFLEX_ACCENT(226, "acirc", 'â'),
  LETTER_SMALL_A_TILDE(227, "atilde", 'ã'),
  LETTER_SMALL_A_UMLAUT_MARK(228, "auml", 'ä'),
  LETTER_SMALL_A_RING(229, "aring", 'å'),
  LETTER_SMALL_AE(230, "aelig", 'æ'),
  LETTER_SMALL_C_CEDILLA(231, "ccedil", 'ç'),
  LETTER_SMALL_E_GRAVE_ACCENT(232, "egrave", 'è'),
  LETTER_SMALL_E_ACUTE_ACCENT(233, "eacute", 'é'),
  LETTER_SMALL_E_CIRCUMFLEX_ACCENT(234, "ecirc", 'ê'),
  LETTER_SMALL_E_UMLAUT_MARK(235, "euml", 'ë'),
  LETTER_SMALL_I_GRAVE_ACCENT(236, "igrave", 'ì'),
  LETTER_SMALL_I_ACUTE_ACCENT(237, "iacute", 'í'),
  LETTER_SMALL_I_CIRCUMFLEX_ACCENT(238, "icirc", 'î'),
  LETTER_SMALL_I_UMLAUT_MARK(239, "iuml", 'ï'),
  LETTER_SMALL_ETH(240, "eth", 'ð'),
  LETTER_SMALL_N_TILDE(241, "ntilde", 'ñ'),
  LETTER_SMALL_O_GRAVE_ACCENT(242, "ograve", 'ò'),
  LETTER_SMALL_O_ACUTE_ACCENT(243, "oacute", 'ó'),
  LETTER_SMALL_O_CIRCUMFLEX_ACCENT(244, "ocirc", 'ô'),
  LETTER_SMALL_O_TILDE(245, "otilde", 'õ'),
  LETTER_SMALL_O_UMLAUT_MARK(246, "ouml", 'ö'),
  SYMBOL_DIVISION(247, "divide", '÷'),
  LETTER_SMALL_O_SLASH(248, "oslash", 'ø'),
  LETTER_SMALL_U_GRAVE_ACCENT(249, "ugrave", 'ù'),
  LETTER_SMALL_U_ACUTE_ACCENT(250, "uacute", 'ú'),
  LETTER_SMALL_U_CIRCUMFLEX_ACCENT(251, "ucirc", 'û'),
  LETTER_SMALL_U_UMLAUT_MARK(252, "uuml", 'ü'),
  LETTER_SMALL_Y_ACUTE_ACCENT(253, "yacute", 'ý'),
  LETTER_SMALL_THORN(254, "thorn", 'þ'),
  LETTER_SMALL_Y_UMLAUT_MARK(255, "yuml", 'ÿ'),
  LETTER_CAPITAL_OE(338, "OElig", 'Œ', 140),
  LETTER_SMALL_OE(339, "oelig", 'œ', 156),
  LETTER_CAPITAL_S_CARON(352, "Scaron", 'Š', 138),
  LETTER_SMALL_S_CARON(353, "scaron", 'š', 154),
  LETTER_CAPITAL_Y_UMLAUT_MARK(376, "Yuml", 'Ÿ', 159),
  LETTER_F_WITH_HOOK(402, "fnof", 'ƒ', 131),
  MODIFIER_LETTER_CIRCUMFLEX_ACCENT(710, "circ", 'ˆ', 136),
  SYMBOL_SMALL_TILDE(732, "tilde", '˜', 152),
  LETTER_CAPITAL_ALPHA(913, "Alpha", 'Α'),
  LETTER_CAPITAL_BETA(914, "Beta", 'Β'),
  LETTER_CAPITAL_GAMMA(915, "Gamma", 'Γ'),
  LETTER_CAPITAL_DELTA(916, "Delta", 'Δ'),
  LETTER_CAPITAL_EPSILON(917, "Epsilon", 'Ε'),
  LETTER_CAPITAL_ZETA(918, "Zeta", 'Ζ'),
  LETTER_CAPITAL_ETA(919, "Eta", 'Η'),
  LETTER_CAPITAL_THETA(920, "Theta", 'Θ'),
  LETTER_CAPITAL_IOTA(921, "Iota", 'Ι'),
  LETTER_CAPITAL_KAPPA(922, "Kappa", 'Κ'),
  LETTER_CAPITAL_LAMBDA(923, "Lambda", 'Λ'),
  LETTER_CAPITAL_MU(924, "Mu", 'Μ'),
  LETTER_CAPITAL_NU(925, "Nu", 'Ν'),
  LETTER_CAPITAL_XI(926, "Xi", 'Ξ'),
  LETTER_CAPITAL_OMICRON(927, "Omicron", 'Ο'),
  LETTER_CAPITAL_PI(928, "Pi", 'Π'),
  LETTER_CAPITAL_RHO(929, "Rho", 'Ρ'),
  LETTER_CAPITAL_SIGMA(931, "Sigma", 'Σ'),
  LETTER_CAPITAL_TAU(932, "Tau", 'Τ'),
  LETTER_CAPITAL_UPSILON(933, "Upsilon", 'Υ'),
  LETTER_CAPITAL_PHI(934, "Phi", 'Φ'),
  LETTER_CAPITAL_CHI(935, "Chi", 'Χ'),
  LETTER_CAPITAL_PSI(936, "Psi", 'Ψ'),
  LETTER_CAPITAL_OMEGA(937, "Omega", 'Ω'),
  LETTER_SMALL_ALPHA(945, "alpha", 'α'),
  LETTER_SMALL_BETA(946, "beta", 'β'),
  LETTER_SMALL_GAMMA(947, "gamma", 'γ'),
  LETTER_SMALL_DELTA(948, "delta", 'δ'),
  LETTER_SMALL_EPSILON(949, "epsilon", 'ε'),
  LETTER_SMALL_ZETA(950, "zeta", 'ζ'),
  LETTER_SMALL_ETA(951, "eta", 'η'),
  LETTER_SMALL_THETA(952, "theta", 'θ'),
  LETTER_SMALL_IOTA(953, "iota", 'ι'),
  LETTER_SMALL_KAPPA(954, "kappa", 'κ'),
  LETTER_SMALL_LAMBDA(955, "lambda", 'λ'),
  LETTER_SMALL_MU(956, "mu", 'μ'),
  LETTER_SMALL_NU(957, "nu", 'ν'),
  LETTER_SMALL_XI(958, "xi", 'ξ'),
  LETTER_SMALL_OMICRON(959, "omicron", 'ο'),
  LETTER_SMALL_PI(960, "pi", 'π'),
  LETTER_SMALL_RHO(961, "rho", 'ρ'),
  LETTER_SMALL_SIGMAF(962, "sigmaf", 'ς'),
  LETTER_SMALL_SIGMA(963, "sigma", 'σ'),
  LETTER_SMALL_TAU(964, "tau", 'τ'),
  LETTER_SMALL_UPSILON(965, "upsilon", 'υ'),
  LETTER_SMALL_PHI(966, "phi", 'φ'),
  LETTER_SMALL_CHI(967, "chi", 'χ'),
  LETTER_SMALL_PSI(968, "psi", 'ψ'),
  LETTER_SMALL_OMEGA(969, "omega", 'ω'),
  LETTER_THETA_SYMBOL(977, "thetasym", 'ϑ'),
  LETTER_UPSILON_SYMBOL(978, "upsih", 'ϒ'),
  LETTER_PI_SYMBOL(982, "piv", 'ϖ'),
  EN_SPACE(8194, "ensp", '\u2002'),
  EM_SPACE(8195, "emsp", '\u2003'),
  THIN_SPACE(8201, "thinsp", '\u2009'),
  ZERO_WIDTH_NON_JOINER(8204, "zwnj", '\u200C'),
  ZERO_WIDTH_JOINER(8205, "zwj", '\u200D'),
  LEFT_TO_RIGHT_MARK(8206, "lrm", '\u200E'),
  RIGHT_TO_LEFT_MARK(8207, "rlm", '\u200F'),
  SYMBOL_UNBREAKABLE_HYPHEN(8209, null, '\u2011', false, false),
  SYMBOL_EN_DASH(8211, "ndash", '–', 150),
  SYMBOL_EM_DASH(8212, "mdash", '—', 151),
  SYMBOL_LEFT_SINGLE_QUOTATION_MARK(8216, "lsquo", '‘', 145),
  SYMBOL_RIGHT_SINGLE_QUOTATION_MARK(8217, "rsquo", '’', 146),
  SYMBOL_SINGLE_LOW_9_QUOTATION_MARK(8218, "sbquo", '‚', 130),
  SYMBOL_LEFT_DOUBLE_QUOTATION_MARK(8220, "ldquo", '“', 147),
  SYMBOL_RIGHT_DOUBLE_QUOTATION_MARK(8221, "rdquo", '”', 148),
  SYMBOL_DOUBLE_LOW_9_QUOTATION_MARK(8222, "bdquo", '„', 132),
  SYMBOL_DAGGER(8224, "dagger", '†', 134),
  SYMBOL_DOUBLE_DAGGER(8225, "Dagger", '‡', 135),
  SYMBOL_BULLET(8226, "bull", '•', 149),
  SYMBOL_HORIZONTAL_ELLIPSIS(8230, "hellip", '…', 133),
  NARROW_NO_BREAK_SPACE(8239, null, '\u202F'),
  SYMBOL_PER_MILLE(8240, "permil", '‰', 137),
  SYMBOL_MINUTES(8242, "prime", '′'),
  SYMBOL_SECONDS(8243, "Prime", '″'),
  SYMBOL_SINGLE_LEFT_ANGLE_QUOTATION(8249, "lsaquo", '‹', 139),
  SYMBOL_SINGLE_RIGHT_ANGLE_QUOTATION(8250, "rsaquo", '›', 155),
  SYMBOL_OVERLINE(8254, "oline", '‾'),
  SYMBOL_FRACTION_SLASH(8260, "frasl", '⁄'),
  SYMBOL_EURO(8364, "euro", '€', 128),
  LETTER_CAPITAL_I_BLACK(8465, "image", '\u2111'),
  LETTER_CAPITAL_P_SCRIPT(8472, "weierp", '\u2118'),
  LETTER_CAPITAL_R_BLACK(8476, "real", '\u211C'),
  SYMBOL_TRADEMARK(8482, "trade", '™', 153),
  SYMBOL_ALEF(9501, "alefsym", '\u2135'),
  SYMBOL_LEFT_ARROW(8592, "larr", '←'),
  SYMBOL_UP_ARROW(8593, "uarr", '↑'),
  SYMBOL_RIGHT_ARROW(8594, "rarr", '→'),
  SYMBOL_DOWN_ARROW(8595, "darr", '↓'),
  SYMBOL_LEFT_RIGHT_ARROW(8596, "harr", '↔'),
  SYMBOL_CARRIAGE_RETURN_ARROW(8629, "crarr", '\u21B5'),
  SYMBOL_LEFT_DOUBLE_ARROW(8656, "lArr", '\u21D0'),
  SYMBOL_UP_DOUBLE_ARROW(8657, "uArr", '\u21D1'),
  SYMBOL_RIGHT_DOUBLE_ARROW(8658, "rArr", '\u21D2'),
  SYMBOL_DOWN_DOUBLE_ARROW(8659, "dArr", '\u21D3'),
  SYMBOL_LEFT_RIGHT_DOUBLE_ARROW(8660, "hArr", '\u21D4'),
  SYMBOL_FOR_ALL(8704, "forall", '\u2200'),
  SYMBOL_PART(8706, "part", '∂'),
  SYMBOL_EXISTS(8707, "exists", '\u2203'),
  SYMBOL_EMPTY(8709, "empty", '\u2205'),
  SYMBOL_NABLA(8711, "nabla", '\u2207'),
  SYMBOL_IS_IN(8712, "isin", '\u2208'),
  SYMBOL_NOT_IN(8713, "notin", '\u2209'),
  SYMBOL_NI(8715, "ni", '\u220B'),
  SYMBOL_PROD(8719, "prod", '∏'),
  SYMBOL_SUM(8721, "sum", '∑'),
  SYMBOL_MINUS(8722, "minus", '−'),
  SYMBOL_LOW_ASTERISK(8727, "lowast", '\u2217'),
  SYMBOL_SQUARE_ROOT(8730, "radic", '√'),
  SYMBOL_PROPORTIONAL_TO(8733, "prop", '\u221D'),
  SYMBOL_INFINITY(8734, "infin", '∞'),
  SYMBOL_ANGLE(8736, "ang", '\u2220'),
  SYMBOL_AND(8743, "and", '\u2227'),
  SYMBOL_OR(8744, "or", '\u2228'),
  SYMBOL_CAP(8745, "cap", '∩'),
  SYMBOL_CUP(8746, "cup", '\u222A'),
  SYMBOL_INTEGRAL(8747, "int", '∫'),
  SYMBOL_THEREFORE(8756, "there4", '\u2234'),
  SYMBOL_SIMILAR_TO(8764, "sim", '\u223C'),
  SYMBOL_CONGRUENT_TO(8773, "cong", '\u2245'),
  SYMBOL_ALMOST_EQUAL(8776, "asymp", '≈'),
  SYMBOL_NOT_EQUAL(8800, "ne", '≠'),
  SYMBOL_EQUIVALENT(8801, "equiv", '≡'),
  SYMBOL_LESS_OR_EQUAL(8804, "le", '≤'),
  SYMBOL_GREATER_OR_EQUAL(8805, "ge", '≥'),
  SYMBOL_SUBSET(8834, "sub", '\u2282'),
  SYMBOL_SUPERSET(8835, "sup", '\u2283'),
  SYMBOL_NOT_SUBSET(8836, "nsub", '\u2284'),
  SYMBOL_SUBSET_OR_EQUAL(8838, "sube", '\u2286'),
  SYMBOL_SUPERSET_OR_EQUAL(8839, "supe", '\u2287'),
  SYMBOL_CIRCLED_PLUS(8853, "oplus", '\u2295'),
  SYMBOL_CIRCLED_TIMES(8855, "otimes", '\u2297'),
  SYMBOL_PERPENDICULAR(8869, "perp", '\u22A5'),
  SYMBOL_DOT_OPERATOR(8901, "sdot", '\u22C5'),
  SYMBOL_LEFT_CEILING(8968, "lceil", '\u2308'),
  SYMBOL_RIGHT_CEILING(8969, "rceil", '\u2309'),
  SYMBOL_LEFT_FLOOR(8970, "lfloor", '\u230A'),
  SYMBOL_RIGHT_FLOOR(8971, "rfloor", '\u230B'),
  SYMBOL_LOZENGE(9674, "loz", '◊'),
  SYMBOL_SPADE(9824, "spades", '♠'),
  SYMBOL_CLUB(9827, "clubs", '♣'),
  SYMBOL_HEART(9829, "hearts", '♥'),
  SYMBOL_DIAMOND(9830, "diams", '♦');

  /**
   * Entity number.
   */
  private final int number;

  /**
   * Alternative entity number.
   */
  private final int alternativeNumber;

  /**
   * Entity name.
   */
  private final String name;

  /**
   * Entity value.
   */
  private final char value;

  /**
   * True if numeric entities should be replaced.
   */
  private final boolean replaceNumeric;

  /**
   * True if named entities should be replaced.
   */
  private final boolean replaceName;

  /**
   * @param number Entity number.
   * @return HTML character for the given entity number.
   */
  public static HtmlCharacters getCharacterByEntityNumber(int number) {
    for (HtmlCharacters character : HtmlCharacters.values()) {
      if (character.getNumber() == number) {
        return character;
      }
    }
    for (HtmlCharacters character : HtmlCharacters.values()) {
      if (character.getAlternativeNumber() == number) {
        return character;
      }
    }
    return null;
  }

  /**
   * @param number Entity number.
   * @param name Entity name.
   * @param value Entity value.
   */
  HtmlCharacters(int number, String name, char value) {
    this.number = number;
    this.alternativeNumber = number;
    this.name = name;
    this.value = value;
    this.replaceNumeric = true;
    this.replaceName = true;
  }

  /**
   * @param number Entity number.
   * @param name Entity name.
   * @param value Entity value.
   * @param alternative Alternative entity number.
   */
  HtmlCharacters(int number, String name, char value, int alternativeNumber) {
    this.number = number;
    this.alternativeNumber = alternativeNumber;
    this.name = name;
    this.value = value;
    this.replaceNumeric = true;
    this.replaceName = true;
  }

  /**
   * @param number Entity number.
   * @param name Entity name.
   * @param value Entity value.
   * @param replaceNumeric True if numeric entities should be replaced.
   * @param replaceName True if named entities should be replaced.
   */
  HtmlCharacters(
      int number, String name, char value,
      boolean replaceNumeric, boolean replaceName) {
    this.number = number;
    this.alternativeNumber = number;
    this.name = name;
    this.value = value;
    this.replaceNumeric = replaceNumeric;
    this.replaceName = replaceName;
  }

  /**
   * @return Entity number.
   */
  public int getNumber() {
    return number;
  }

  /**
   * @return Alternative entity number.
   */
  public int getAlternativeNumber() {
    return alternativeNumber;
  }

  /**
   * @return Entity name.
   */
  public String getName() {
    return name;
  }

  /**
   * @return Full entity in HTML format.
   */
  public String getFullEntity() {
    if (name != null) {
      return "&" + name + ";";
    }
    return null;
  }

  /**
   * @return Entity value;
   */
  public char getValue() {
    return value;
  }

  /**
   * @return True if numeric entities should be replaced.
   */
  public boolean shouldReplaceNumeric() {
    return replaceNumeric;
  }

  /**
   * @return True if named entities should be replaced.
   */
  public boolean shouldReplaceName() {
    return replaceName;
  }
}
