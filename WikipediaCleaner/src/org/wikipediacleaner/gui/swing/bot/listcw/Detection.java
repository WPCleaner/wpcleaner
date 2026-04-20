package org.wikipediacleaner.gui.swing.bot.listcw;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Page;

import java.util.ArrayList;
import java.util.List;

/**
 * Bean for holding detection results.
 */
class Detection implements Comparable<Detection> {

  /**
   * Namespace
   */
  public final Integer namespace;

  /**
   * Page name
   */
  public final String pageName;

  /**
   * List of notices
   */
  public final List<String> notices;

  /**
   * Maximum level for the errors
   */
  public final CheckErrorResult.ErrorLevel maxLevel;

  /**
   * @param page   Page.
   * @param errors List of errors.
   */
  public Detection(Page page, List<CheckErrorResult> errors) {
    this.namespace = page.getNamespace();
    this.pageName = page.getTitle();
    this.notices = new ArrayList<>();
    CheckErrorResult.ErrorLevel tmpLevel = CheckErrorResult.ErrorLevel.CORRECT;
    if (errors != null) {
      for (CheckErrorResult error : errors) {
        String contents = page.getContents();
        if (contents != null) {
          notices.add(new String(contents.substring(
              error.getStartPosition(), error.getEndPosition())));
        }
        CheckErrorResult.ErrorLevel currentLevel = error.getErrorLevel();
        if (currentLevel.ordinal() < tmpLevel.ordinal()) {
          tmpLevel = currentLevel;
        }
      }
    }
    this.maxLevel = tmpLevel;
  }

  @Override
  public int compareTo(Detection o) {
    if (o == null) {
      return -1;
    }

    // Compare error level
    if (!maxLevel.equals(o.maxLevel)) {
      if (maxLevel.ordinal() < o.maxLevel.ordinal()) {
        return -1;
      }
      return 1;
    }

    // Compare namespaces
    if (namespace == null) {
      if (o.namespace != null) {
        return 1;
      }
    }
    if (o.namespace == null) {
      return -1;
    }
    if (!namespace.equals(o.namespace)) {
      return namespace.compareTo(o.namespace);
    }

    // Compare pages
    if (pageName == null) {
      if (o.pageName == null) {
        return 0;
      }
      return 1;
    }
    if (o.pageName == null) {
      return -1;
    }
    return pageName.compareTo(o.pageName);
  }
}
