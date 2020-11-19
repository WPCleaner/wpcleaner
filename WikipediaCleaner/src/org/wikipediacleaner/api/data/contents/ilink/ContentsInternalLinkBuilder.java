/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.ilink;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.contents.ContentsUtil;

/**
 * Builder class.
 */
public class ContentsInternalLinkBuilder {

  /** Target of the link */
  @Nonnull
  private final String target;

  /** Is colon mandatory? */
  private boolean colon;

  /** Anchor for the target */
  @Nullable
  private String anchor;

  /** Text of the link */
  @Nullable
  private String text;

  /**
   * Private constructor.
   * 
   * @param target Target of the link.
   */
  private ContentsInternalLinkBuilder(@Nonnull String target) {
    this.target = target;
  }

  /**
   * Initialize a builder with the target of the link.
   * 
   * @param target Target of the link.
   * @return Builder initialized with the target of the link.
   */
  public static @Nonnull ContentsInternalLinkBuilder from(@Nonnull String target) {
    ContentsInternalLinkBuilder builder = new ContentsInternalLinkBuilder(target);
    return builder;
  }

  /**
   * @param withColon True if the colon is mandatory.
   * @return Builder.
   */
  public ContentsInternalLinkBuilder withColon(boolean withColon) {
    this.colon = withColon;
    return this;
  }

  /**
   * @param withAnchor Anchor of the target. 
   * @return Builder.
   */
  public ContentsInternalLinkBuilder withAnchor(@Nullable String withAnchor) {
    this.anchor = (withAnchor != null) ? withAnchor.trim() : null;
    return this;
  }

  /**
   * @param withText Text of the link.
   * @return Builder.
   */
  public ContentsInternalLinkBuilder withText(@Nullable String withText) {
    this.text = (withText != null) ? withText.trim() : null;
    return this;
  }

  /**
   * @return Textual representation of the link.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    // Construct complete target
    String fullTarget = StringUtils.defaultIfEmpty(target, "");
    if (anchor != null) {
      fullTarget += "#" + anchor;
    }
    if (colon ||
        ((fullTarget.length() > 0) && (fullTarget.charAt(0) == '/'))) {
      fullTarget = ":" + fullTarget;
    }

    // Build link
    if (text == null) {
      return String.format(
          "[[%s]]",
          fullTarget);
    }
    if (StringUtils.isEmpty(fullTarget) || StringUtils.isEmpty(text)) {
      return String.format(
          "[[%s|%s]]",
          fullTarget,
          text);
    }
    boolean canBeSimplified = false;
    if ((text.length() >= fullTarget.length()) &&
        Page.areSameTitle(fullTarget, text.substring(0, fullTarget.length()))) {
      int lastIndex = ContentsUtil.moveIndexForwardWhileFound(
          text,
          fullTarget.length(),
          PageElementInternalLink.EXTENSION_CHARACTERS);
      if (lastIndex >= text.length()) {
        canBeSimplified = true;
      }
    }
    if (canBeSimplified) {
      return String.format(
          "[[%s]]%s",
          text.substring(0, fullTarget.length()),
          text.substring(fullTarget.length()));
    }
    return String.format(
        "[[%s|%s]]",
        fullTarget,
        text);
  }
}
