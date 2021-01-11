/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.tag;

import javax.annotation.Nonnull;

/**
 * Definition of a WIKI tag type.
 */
public class WikiTagType extends TagType {

  // Normalized names
  private static final String CHEM_NAME            = "chem";

  private static final String GALLERY_NAME         = "gallery";
  private static final String GRAPH_NAME           = "graph";

  private static final String HIERO_NAME           = "hiero";

  private static final String IMAGEMAP_NAME        = "imagemap";
  private static final String INCLUDEONLY_NAME     = "includeonly";

  private static final String MAPFRAME_NAME        = "mapframe";
  private static final String MATH_NAME            = "math";
  private static final String MATH_CHEM_NAME       = "ce"; // Shortcut for math chem

  private static final String NOINCLUDE_NAME       = "noinclude";
  private static final String NOWIKI_NAME          = "nowiki";

  private static final String ONLYINCLUDE_NAME     = "onlyinclude";

  private static final String PRE_NAME             = "pre";

  private static final String REF_NAME             = "ref";
  private static final String REFERENCES_NAME      = "references";

  private static final String SCORE_NAME           = "score";
  private static final String SOURCE_NAME          = "source";
  private static final String SYNTAXHIGHLIGHT_NAME = "syntaxhighlight";

  private static final String TEMPLATEDATA_NAME    = "templatedata";
  private static final String TIMELINE_NAME        = "timeline";

  // Normalized tags
  public static final WikiTagType CHEM            = createRegularTag(CHEM_NAME);

  public static final WikiTagType GALLERY         = createRegularTag(GALLERY_NAME);
  public static final WikiTagType GRAPH           = createRegularTag(GRAPH_NAME);

  public static final WikiTagType HIERO           = createRegularTag(HIERO_NAME);

  public static final WikiTagType IMAGEMAP        = createRegularTag(IMAGEMAP_NAME);
  public static final WikiTagType INCLUDEONLY     = createRegularTag(INCLUDEONLY_NAME);

  public static final WikiTagType MAPFRAME        = createRegularTag(MAPFRAME_NAME);
  public static final WikiTagType MATH            = createRegularTag(MATH_NAME);
  public static final WikiTagType MATH_CHEM       = createRegularTag(MATH_CHEM_NAME);

  public static final WikiTagType NOINCLUDE       = createRegularTag(NOINCLUDE_NAME);
  public static final WikiTagType NOWIKI          = createRegularTag(NOWIKI_NAME);

  public static final WikiTagType ONLYINCLUDE     = createRegularTag(ONLYINCLUDE_NAME);

  public static final WikiTagType PRE             = createRegularTag(PRE_NAME);

  public static final WikiTagType REF             = createRegularTag(REF_NAME);
  public static final WikiTagType REFERENCES      = createRegularTag(REFERENCES_NAME);

  public static final WikiTagType SCORE           = createRegularTag(SCORE_NAME);
  public static final WikiTagType SOURCE          = createRegularTag(SOURCE_NAME);
  public static final WikiTagType SYNTAXHIGHLIGHT = createRegularTag(SYNTAXHIGHLIGHT_NAME);

  public static final WikiTagType TEMPLATEDATA    = createRegularTag(TEMPLATEDATA_NAME);
  public static final WikiTagType TIMELINE        = createRegularTag(TIMELINE_NAME);

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
   */
  private static WikiTagType createRegularTag(@Nonnull String name) {
    return new WikiTagType(name, true, true, true, false);
  }

  /**
   * Create a WIKI tag type.
   * 
   * @param name Normalized name of the tag type.
   * @param openPossible True if the tag can be open.
   * @param closePossible True if the tag can be close.
   * @param fullPossible True if the tag can be full.
   * @param unclosedOk True if an open tag can normally be without a close tag.
   */
  private WikiTagType(
      @Nonnull String name,
      boolean openPossible,
      boolean closePossible,
      boolean fullPossible,
      boolean unclosedOk) {
    super(name, openPossible, closePossible, fullPossible, unclosedOk);
  }
}
