/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.magicword;

import javax.annotation.Nonnull;

/**
 * Definition of an image magic word type.
 */
public class ImageMagicWordType extends MagicWordType {

  // Names
  private static final String ID_IMG_ALT = "img_alt";
  private static final String ID_IMG_BASELINE = "img_baseline";
  private static final String ID_IMG_BORDER = "img_border";
  private static final String ID_IMG_BOTTOM = "img_bottom";
  private static final String ID_IMG_CENTER = "img_center";
  private static final String ID_IMG_CLASS = "img_class";
  private static final String ID_IMG_FRAMED = "img_framed";
  private static final String ID_IMG_FRAMELESS = "img_frameless";
  private static final String ID_IMG_LANG = "img_lang";
  private static final String ID_IMG_LEFT = "img_left";
  private static final String ID_IMG_LINK = "img_link";
  private static final String ID_IMG_LOSSY = "img_lossy";
  private static final String ID_IMG_MANUAL_THUMB = "img_manualthumb";
  private static final String ID_IMG_MIDDLE = "img_middle";
  private static final String ID_IMG_NONE = "img_none";
  private static final String ID_IMG_PAGE = "img_page";
  private static final String ID_IMG_RIGHT = "img_right";
  private static final String ID_IMG_SUB = "img_sub";
  private static final String ID_IMG_SUPER = "img_super";
  private static final String ID_IMG_TEXT_BOTTOM = "img_text_bottom";
  private static final String ID_IMG_TEXT_TOP = "img_text_top";
  private static final String ID_IMG_THUMBNAIL = "img_thumbnail";
  private static final String ID_IMG_TOP = "img_top";
  private static final String ID_IMG_UPRIGHT = "img_upright";
  private static final String ID_IMG_WIDTH = "img_width";
  private static final String ID_TIMED_MEDIA_ENDTIME = "timedmedia_endtime";
  private static final String ID_TIMED_MEDIA_NOICON = "timedmedia_noicon";
  private static final String ID_TIMED_MEDIA_NOPLAYER = "timedmedia_noplayer";
  private static final String ID_TIMED_MEDIA_STARTTIME = "timedmedia_starttime";
  private static final String ID_TIMED_MEDIA_THUMBTIME = "timedmedia_thumbtime";

  // Magic words
  public static final ImageMagicWordType IMG_ALT = createImageType(ID_IMG_ALT);
  public static final ImageMagicWordType IMG_BASELINE = createImageType(ID_IMG_BASELINE);
  public static final ImageMagicWordType IMG_BORDER = createImageType(ID_IMG_BORDER);
  public static final ImageMagicWordType IMG_BOTTOM = createImageType(ID_IMG_BOTTOM);
  public static final ImageMagicWordType IMG_CENTER = createImageType(ID_IMG_CENTER);
  public static final ImageMagicWordType IMG_CLASS = createImageType(ID_IMG_CLASS);
  public static final ImageMagicWordType IMG_FRAMED = createImageType(ID_IMG_FRAMED);
  public static final ImageMagicWordType IMG_FRAMELESS = createImageType(ID_IMG_FRAMELESS);
  public static final ImageMagicWordType IMG_LANG = createImageType(ID_IMG_LANG);
  public static final ImageMagicWordType IMG_LEFT = createImageType(ID_IMG_LEFT);
  public static final ImageMagicWordType IMG_LINK = createImageType(ID_IMG_LINK);
  public static final ImageMagicWordType IMG_LOSSY = createImageType(ID_IMG_LOSSY);
  public static final ImageMagicWordType IMG_MANUAL_THUMB = createImageType(ID_IMG_MANUAL_THUMB);
  public static final ImageMagicWordType IMG_MIDDLE = createImageType(ID_IMG_MIDDLE);
  public static final ImageMagicWordType IMG_NONE = createImageType(ID_IMG_NONE);
  public static final ImageMagicWordType IMG_PAGE = createImageType(ID_IMG_PAGE);
  public static final ImageMagicWordType IMG_RIGHT = createImageType(ID_IMG_RIGHT);
  public static final ImageMagicWordType IMG_SUB = createImageType(ID_IMG_SUB);
  public static final ImageMagicWordType IMG_SUPER = createImageType(ID_IMG_SUPER);
  public static final ImageMagicWordType IMG_TEXT_BOTTOM = createImageType(ID_IMG_TEXT_BOTTOM);
  public static final ImageMagicWordType IMG_TEXT_TOP = createImageType(ID_IMG_TEXT_TOP);
  public static final ImageMagicWordType IMG_THUMBNAIL = createImageType(ID_IMG_THUMBNAIL);
  public static final ImageMagicWordType IMG_TOP = createImageType(ID_IMG_TOP);
  public static final ImageMagicWordType IMG_UPRIGHT = createImageType(ID_IMG_UPRIGHT);
  public static final ImageMagicWordType IMG_WIDTH = createImageType(ID_IMG_WIDTH);
  public static final ImageMagicWordType TIMED_MEDIA_ENDTIME = createImageType(ID_TIMED_MEDIA_ENDTIME);
  public static final ImageMagicWordType TIMED_MEDIA_NOICON = createImageType(ID_TIMED_MEDIA_NOICON);
  public static final ImageMagicWordType TIMED_MEDIA_NOPLAYER = createImageType(ID_TIMED_MEDIA_NOPLAYER);
  public static final ImageMagicWordType TIMED_MEDIA_STARTTIME = createImageType(ID_TIMED_MEDIA_STARTTIME);
  public static final ImageMagicWordType TIMED_MEDIA_THUMBTIME = createImageType(ID_TIMED_MEDIA_THUMBTIME);

  /**
   * Register magic word types.
   */
  static void registerMagicWordTypes() {
    // Do nothing, magic words register by themselves
  }

  /**
   * Create an image magic word type.
   * 
   * @param name Name of the magic word type.
   * @return Magic word type.
   */
  private static ImageMagicWordType createImageType(@Nonnull String name) {
    return new ImageMagicWordType(name);
  }

  /**
   * Create an image magic word type.
   * 
   * @param name Name of the magic word type.
   */
  private ImageMagicWordType(
      @Nonnull String name) {
    super(name, true, false, false, false);
  }

}
