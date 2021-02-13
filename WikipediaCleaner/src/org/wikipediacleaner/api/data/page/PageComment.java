/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data.page;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.utils.Configuration;

/**
 * A class to save comments and information for a Page.
 */
public class PageComment {

  @Nonnull private final EnumWikipedia wiki;
  @Nonnull private final String pageTitle;
  @Nullable private String comment;
  @Nullable private Integer maxArticles;
  @Nullable private Integer maxMainArticles;
  @Nullable private Integer maxOtherArticles;
  @Nullable private Integer maxTemplateArticles;

  /**
   * Retrieve comments and information for a page.
   * 
   * @param wiki Wiki.
   * @param pageTitle Page title.
   * @return Optional information.
   */
  @Nonnull
  public static Optional<PageComment> get(
      @Nonnull final EnumWikipedia wiki,
      @Nonnull final String pageTitle) {
    Configuration config = Configuration.getConfiguration();
    Object tmp = config.getPojo(
        wiki, Configuration.POJO_PAGE_COMMENTS, pageTitle, () -> new PageComment(wiki, pageTitle));
    if (tmp instanceof PageComment) {
      PageComment comment = new PageComment(wiki, pageTitle);
      comment.comment = ((PageComment) tmp).comment;
      comment.maxArticles = ((PageComment) tmp).maxArticles;
      comment.maxMainArticles = ((PageComment) tmp).maxMainArticles;
      comment.maxOtherArticles = ((PageComment) tmp).maxOtherArticles;
      comment.maxTemplateArticles = ((PageComment) tmp).maxTemplateArticles;
      return Optional.of(comment);
    }
    return Optional.empty();
  }

  /**
   * Retrieve comments and information for a list of pages.
   * 
   * @param wiki Wiki.
   * @param pages List of pages.
   * @return Map of comments and information indexed by page title.
   */
  @Nonnull
  public static Map<String, PageComment> get(
      @Nonnull final EnumWikipedia wiki,
      @Nonnull final Collection<Page> pages) {
    final Map<String, PageComment> result = new HashMap<>();
    pages.forEach(page -> get(wiki, page.getTitle()).ifPresent(comment -> result.put(page.getTitle(), comment)));
    return result;
  }

  /**
   * Retrieve or create comments and information for a page.
   * 
   * @param wiki Wiki.
   * @param pageTitle Page title.
   * @return Optional information.
   */
  @Nonnull
  public static PageComment getOrCreate(
      @Nonnull final EnumWikipedia wiki,
      @Nonnull final String pageTitle) {
    return get(wiki, pageTitle).orElseGet(() -> new PageComment(wiki, pageTitle));
  }

  /**
   * Delete comments and information for a page.
   * 
   * @param wiki Wiki.
   * @param pageTitle Page title.
   */
  public static void delete(
      @Nonnull final EnumWikipedia wiki,
      @Nonnull final String pageTitle) {
    Configuration config = Configuration.getConfiguration();
    config.removePojo(wiki, Configuration.POJO_PAGE_COMMENTS, pageTitle);
  }

  /**
   * Create a bean for storing comments and information for a page.
   * 
   * @param wiki Wiki.
   * @param pageTitle Page title.
   */
  private PageComment(
      @Nonnull final EnumWikipedia wiki,
      @Nonnull final String pageTitle) {
    this.wiki = wiki;
    this.pageTitle = pageTitle;
  }

  /**
   * @return Comments on the page.
   */
  @Nonnull
  public Optional<String> getComment() {
    return Optional.ofNullable(comment);
  }

  /**
   * @param comment Comments on the page.
   */
  public void setComment(String comment) {
    this.comment = comment;
  }

  /**
   * @return Maximum number of back links from the main name space.
   */
  @Nonnull
  public Optional<Integer> getMaxMainArticles() {
    return Optional.ofNullable(maxMainArticles);
  }

  /**
   * @param max Maximum number of back links from the main name space.
   */
  public void setMaxMainArticles(Integer max) {
    this.maxMainArticles = max;
  }

  /**
   * @return Maximum number of back links from the template name space.
   */
  @Nonnull
  public Optional<Integer> getMaxTemplateArticles() {
    return Optional.ofNullable(maxTemplateArticles);
  }

  /**
   * @param max Maximum number of back links from the template name space.
   */
  public void setMaxTemplateArticles(Integer max) {
    this.maxTemplateArticles = max;
  }

  /**
   * @return Maximum number of back links from other name spaces.
   */
  @Nonnull
  public Optional<Integer> getMaxOtherArticles() {
    return Optional.ofNullable(maxOtherArticles);
  }

  /**
   * @param max Maximum number of back links from other name spaces.s
   */
  public void setMaxOtherArticles(Integer max) {
    this.maxOtherArticles = max;
  }

  /**
   * @return Maximum number of back links.
   */
  @Nonnull
  public Optional<Integer> computeMaxArticles() {
    if ((maxMainArticles == null) &&
        (maxTemplateArticles == null) &&
        (maxOtherArticles == null)) {
      return Optional.empty();
    }
    return Optional.of(Integer.valueOf(
        (maxMainArticles != null ? maxMainArticles.intValue() : 0) +
        (maxTemplateArticles != null ? maxTemplateArticles.intValue() : 0) +
        (maxOtherArticles != null ? maxOtherArticles.intValue() : 0)));
  }

  /**
   * Compute correct values.
   */
  public void fixValues() {
    if ((maxArticles != null) && (maxOtherArticles == null)) {
      int tmp = maxArticles.intValue();
      if (maxMainArticles != null) {
        tmp -= maxMainArticles.intValue();
      }
      if (maxTemplateArticles != null) {
        tmp -= maxTemplateArticles.intValue();
      }
      maxOtherArticles = Integer.valueOf(tmp);
    }
  }

  /**
   * Save the comments and information.
   */
  public void save() {
    Configuration config = Configuration.getConfiguration();
    config.addPojo(wiki, Configuration.POJO_PAGE_COMMENTS, comment, pageTitle);
  }
}
