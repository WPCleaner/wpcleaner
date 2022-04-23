/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a53x.a534;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.wikipediacleaner.api.data.contents.magicword.ImageMagicWordType;

/**
 * Factory for automatic replacements 
 */
public class AutomaticReplacementFactory {

  /** List of non-existing options */
  private final static List<AutomaticReplacement> nonExistingOptions = Stream
      .of("align", "alt", "caption", "default", "file", "horizontal", "landscape", "largeurpx", "links",
          "logo", "maxi", "nothumb", "panorama", "portrait", "small", "text", "title", "vertical", "view",
          "wide", "widthpx")
      .map(text -> new AutomaticReplacement(text, null, null, true))
      .collect(Collectors.toList());

  /** List of replacements for IMG_BORDER */
  private final static List<AutomaticReplacement> borderReplacements = Stream
      .of("bordo", "rand")
      .map(text -> new AutomaticReplacement(text, ImageMagicWordType.IMG_BORDER, "border", false))
      .collect(Collectors.toList());

  /** List of replacements for IMG_CENTER */
  private final static List<AutomaticReplacement> centerReplacements = Stream
      .of("align=center", "align:center", "centro")
      .map(text -> new AutomaticReplacement(text, ImageMagicWordType.IMG_CENTER, "center", true))
      .collect(Collectors.toList());

  /** List of replacements for IMG_FRAMELESS */
  private final static List<AutomaticReplacement> framelessReplacements = Stream
      .of("rahmenlos", "безрамки")
      .map(text -> new AutomaticReplacement(text, ImageMagicWordType.IMG_FRAMELESS, "frameless", true))
      .collect(Collectors.toList());

  /** List of replacements for IMG_LEFT */
  private final static List<AutomaticReplacement> leftReplacements = Stream
      .of("align=left", "align:left", "esquerda", "esquerra", "gauche",
          "izquierda", "leftt", "sinistra", "ліворуч", "שמאל")
      .map(text -> new AutomaticReplacement(text, ImageMagicWordType.IMG_LEFT, "left", true))
      .collect(Collectors.toList());

  /** List of replacements for IMG_RIGHT */
  private final static List<AutomaticReplacement> rightReplacements = Stream
      .of("align=right", "align:right", "derecha", "desno", "destra",
          "direita", "dreta", "droit", "float right", "float=right", "float:right",
          "floatright", "ight", "rechts", "reght", "rght", "ribght", "richt",
          "righ", "righjt", "righr", "righte", "rightg", "rightl", "rightt",
          "rightx", "righty", "right1", "right2", "righy", "righyt", "rigjt",
          "rignt", "rigt", "rigth", "rigtht", "rihgt", "roght", "rught",
          "праворуч", "дясно", "справа")
      .map(text -> new AutomaticReplacement(text, ImageMagicWordType.IMG_RIGHT, "right", true))
      .collect(Collectors.toList());

  /** List of replacements for IMG_THUMBNAIL */
  private final static List<AutomaticReplacement> thumbnailReplacements = Stream
      .of("mini", "miniatur", "miniatura", "miniaturadaimagem", "miniaturadeimagen", "miniature",
          "miniatuur", "miniatyr", "thum", "thump", "tuhmb", "tumb", "мини", "ממוזער")
      .map(text -> new AutomaticReplacement(text, ImageMagicWordType.IMG_THUMBNAIL, "thumb", true))
      .collect(Collectors.toList());

  /** List of replacements for IMG_UPRIGHT */
  private final static List<AutomaticReplacement> uprightReplacements = Stream
      .of("align=upright", "hochkant", "uoright", "upleft", "uprighht",
          "uprigt", "uprigth", "uptight")
      .map(text -> new AutomaticReplacement(text, ImageMagicWordType.IMG_UPRIGHT, "upright", true))
      .collect(Collectors.toList());

  /** List of all automatic replacements */
  public final static List<AutomaticReplacement> LIST = createAutomaticReplacements();

  /**
   * @return List of automatic replacements.
   */
  private static List<AutomaticReplacement> createAutomaticReplacements() {
    List<AutomaticReplacement> result = new ArrayList<>();
    result.addAll(nonExistingOptions);
    result.addAll(borderReplacements);
    result.addAll(centerReplacements);
    result.addAll(framelessReplacements);
    result.addAll(leftReplacements);
    result.addAll(rightReplacements);
    result.addAll(thumbnailReplacements);
    result.addAll(uprightReplacements);
    return Collections.unmodifiableList(result);
  }

  private AutomaticReplacementFactory() {
  }
}
