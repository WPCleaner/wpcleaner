/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.tag;

import javax.annotation.Nonnull;

/**
 * Definition of an HTML tag type.
 */
public class HtmlTagType extends TagType {

  // Normalized names
  private static final String A_NAME          = "a";
  private static final String ABBR_NAME       = "abbr";
  private static final String AREA_NAME       = "area";

  private static final String B_NAME          = "b";
  private static final String BASE_NAME       = "base";
  private static final String BIG_NAME        = "big";
  private static final String BLOCKQUOTE_NAME = "blockquote";
  private static final String BR_NAME         = "br";

  private static final String CENTER_NAME     = "center";
  private static final String CITE_NAME       = "cite";
  private static final String CODE_NAME       = "code";
  private static final String COL_NAME        = "col";
  private static final String COMMAND_NAME    = "command";

  private static final String DEL_NAME        = "del";
  private static final String DFN_NAME        = "dfn";
  private static final String DIV_NAME        = "div";

  private static final String EM_NAME         = "em";
  private static final String EMBED_NAME      = "embed";

  private static final String FONT_NAME       = "font";

  private static final String H1_NAME         = "h1";
  private static final String H2_NAME         = "h2";
  private static final String H3_NAME         = "h3";
  private static final String H4_NAME         = "h4";
  private static final String H5_NAME         = "h5";
  private static final String H6_NAME         = "h6";
  private static final String H7_NAME         = "h7";
  private static final String H8_NAME         = "h8";
  private static final String H9_NAME         = "h9";
  private static final String HR_NAME         = "hr";

  private static final String I_NAME          = "i";
  private static final String IMG_NAME        = "img";
  private static final String INPUT_NAME      = "input";

  private static final String KBD_NAME        = "kbd";
  private static final String KEYGEN_NAME     = "keygen";

  private static final String LI_NAME         = "li";
  private static final String LINK_NAME       = "link";

  private static final String META_NAME       = "meta";

  private static final String OL_NAME         = "ol";

  private static final String P_NAME          = "p";
  private static final String PARAM_NAME      = "param";

  private static final String S_NAME          = "s";
  private static final String SAMP_NAME       = "samp";
  private static final String SMALL_NAME      = "small";
  // private static final String SOURCE_NAME  = "source";
  private static final String SPAN_NAME       = "span";
  private static final String STRIKE_NAME     = "strike";
  private static final String STRONG_NAME     = "strong";
  private static final String SUB_NAME        = "sub";
  private static final String SUP_NAME        = "sup";

  private static final String TABLE_NAME      = "table";
  private static final String TD_NAME         = "td";
  private static final String TH_NAME         = "th";
  private static final String TR_NAME         = "tr";
  private static final String TRACK_NAME      = "track";
  private static final String TT_NAME         = "tt";

  private static final String U_NAME          = "u";
  private static final String UL_NAME         = "ul";

  private static final String VAR_NAME        = "var";

  private static final String WBR_NAME        = "wbr";

  // Normalized tags
  public static final HtmlTagType A          = createRegularTag(A_NAME);
  public static final HtmlTagType ABBR       = createRegularTag(ABBR_NAME);
  public static final HtmlTagType AREA       = createVoidTag(AREA_NAME);

  public static final HtmlTagType BASE       = createVoidTag(BASE_NAME);
  public static final HtmlTagType B          = createRegularTag(B_NAME);
  public static final HtmlTagType BIG        = createRegularTag(BIG_NAME);
  public static final HtmlTagType BLOCKQUOTE = createRegularTag(BLOCKQUOTE_NAME);
  public static final HtmlTagType BR         = createVoidTag(BR_NAME);

  public static final HtmlTagType CENTER     = createRegularTag(CENTER_NAME);
  public static final HtmlTagType CITE       = createRegularTag(CITE_NAME);
  public static final HtmlTagType CODE       = createRegularTag(CODE_NAME);
  public static final HtmlTagType COL        = createVoidTag(COL_NAME);
  public static final HtmlTagType COMMAND    = createVoidTag(COMMAND_NAME);

  public static final HtmlTagType DEL        = createRegularTag(DEL_NAME);
  public static final HtmlTagType DFN        = createRegularTag(DFN_NAME);
  public static final HtmlTagType DIV        = createRegularTag(DIV_NAME);

  public static final HtmlTagType EM         = createRegularTag(EM_NAME);
  public static final HtmlTagType EMBED      = createVoidTag(EMBED_NAME);

  public static final HtmlTagType FONT       = createRegularTag(FONT_NAME);

  public static final HtmlTagType H1         = createRegularTag(H1_NAME);
  public static final HtmlTagType H2         = createRegularTag(H2_NAME);
  public static final HtmlTagType H3         = createRegularTag(H3_NAME);
  public static final HtmlTagType H4         = createRegularTag(H4_NAME);
  public static final HtmlTagType H5         = createRegularTag(H5_NAME);
  public static final HtmlTagType H6         = createRegularTag(H6_NAME);
  public static final HtmlTagType H7         = createRegularTag(H7_NAME);
  public static final HtmlTagType H8         = createRegularTag(H8_NAME);
  public static final HtmlTagType H9         = createRegularTag(H9_NAME);
  public static final HtmlTagType HR         = createVoidTag(HR_NAME);

  public static final HtmlTagType I          = createRegularTag(I_NAME);
  public static final HtmlTagType IMG        = createVoidTag(IMG_NAME);
  public static final HtmlTagType INPUT      = createVoidTag(INPUT_NAME);

  public static final HtmlTagType KBD        = createRegularTag(KBD_NAME);
  public static final HtmlTagType KEYGEN     = createVoidTag(KEYGEN_NAME);

  public static final HtmlTagType LI         = createRegularTag(LI_NAME);
  public static final HtmlTagType LINK       = createVoidTag(LINK_NAME);

  public static final HtmlTagType META       = createVoidTag(META_NAME);

  public static final HtmlTagType OL         = createRegularTag(OL_NAME);

  public static final HtmlTagType P          = createRegularTag(P_NAME);
  public static final HtmlTagType PARAM      = createVoidTag(PARAM_NAME);

  public static final HtmlTagType S          = createRegularTag(S_NAME);
  public static final HtmlTagType SAMP       = createRegularTag(SAMP_NAME);
  public static final HtmlTagType SMALL      = createRegularTag(SMALL_NAME);
  public static final HtmlTagType SPAN       = createRegularTag(SPAN_NAME);
  public static final HtmlTagType STRIKE     = createRegularTag(STRIKE_NAME);
  public static final HtmlTagType STRONG     = createRegularTag(STRONG_NAME);
  public static final HtmlTagType SUB        = createRegularTag(SUB_NAME);
  public static final HtmlTagType SUP        = createRegularTag(SUP_NAME);

  public static final HtmlTagType TABLE      = createRegularTag(TABLE_NAME);
  public static final HtmlTagType TD         = createRegularTag(TD_NAME);
  public static final HtmlTagType TH         = createRegularTag(TH_NAME);
  public static final HtmlTagType TR         = createRegularTag(TR_NAME);
  public static final HtmlTagType TRACK      = createVoidTag(TRACK_NAME);
  public static final HtmlTagType TT         = createRegularTag(TT_NAME);

  public static final HtmlTagType U          = createRegularTag(U_NAME);
  public static final HtmlTagType UL         = createRegularTag(UL_NAME);

  public static final HtmlTagType VAR        = createRegularTag(VAR_NAME);

  public static final HtmlTagType WBR        = createVoidTag(WBR_NAME);

  /**
   * Register tag types.
   */
  static void registerTagTypes() {
    // Do nothing, tags register by themselves
  }

  /**
   * Create a regular tag.
   * 
   * @param name Normalized name of the tag type.
   * @return Tag type.
   * @see <a href="https://www.w3.org/TR/2011/WD-html-markup-20110113/syntax.html#syntax-elements">W3C HTML syntax#Elements</a>
   */
  private static HtmlTagType createRegularTag(@Nonnull String name) {
    return new HtmlTagType(name, true, true, true, false);
  }

  /**
   * Create a void tag.
   * 
   * @param name Normalized name of the tag type.
   * @return Tag type.
   * @see <a href="https://www.w3.org/TR/2011/WD-html-markup-20110113/syntax.html#syntax-elements">W3C HTML syntax#Elements</a>
   */
  private static HtmlTagType createVoidTag(@Nonnull String name) {
    return new HtmlTagType(name, true, false, true, true);
  }

  /**
   * Create a HTML tag type.
   * 
   * @param name Normalized name of the tag type.
   * @param openPossible True if the tag can be open.
   * @param closePossible True if the tag can be close.
   * @param fullPossible True if the tag can be full.
   * @param unclosedOk True if an open tag can normally be without a close tag.
   */
  private HtmlTagType(
      @Nonnull String name,
      boolean openPossible,
      boolean closePossible,
      boolean fullPossible,
      boolean unclosedOk) {
    super(name, openPossible, closePossible, fullPossible, unclosedOk);
  }
}
