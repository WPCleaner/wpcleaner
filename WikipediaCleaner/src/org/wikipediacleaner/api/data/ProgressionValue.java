/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;


/**
 * Simple class to store current progression. 
 */
public class ProgressionValue implements Comparable<ProgressionValue> {

  private Integer currentValue;
  private Integer goalValue;
  private final boolean displayZero;

  /**
   * @param current Current value.
   * @param goal Goal value.
   * @param displayZero True if a value of zero should be displayed.
   */
  public ProgressionValue(Integer current, Integer goal, boolean displayZero) {
    this.currentValue = current;
    this.goalValue = goal;
    this.displayZero = displayZero;
  }

  public Integer getCurrent() {
    return currentValue;
  }

  public void setCurrent(Integer current) {
    currentValue = current;
  }

  public Integer getGoal() {
    return goalValue;
  }

  public void setGoal(Integer goal) {
    goalValue = goal;
  }

  public int getStatus() {
    if ((currentValue != null) && (goalValue != null)) {
      return currentValue.compareTo(goalValue);
    }
    return 0;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    if (currentValue != null) {
      if ((goalValue != null) && (!currentValue.equals(goalValue))) {
        return "" + currentValue + " / " + goalValue;
      }
      if (!displayZero && (currentValue.intValue() == 0) && (goalValue == null)) {
        return "";
      }
      return currentValue.toString();
    }
    return "";
  }

  /* (non-Javadoc)
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof ProgressionValue)) {
      return false;
    }
    ProgressionValue value = (ProgressionValue) obj;
    if (currentValue == null) {
      if (value.currentValue != null) {
        return false;
      }
    } else {
      if (!currentValue.equals(value.currentValue)) {
        return false;
      }
    }
    if (goalValue == null) {
      if (value.goalValue != null) {
        return false;
      }
    } else {
      if (!goalValue.equals(value.goalValue)) {
        return false;
      }
    }
    return true;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode() {
    int hashCode = 0;
    if (currentValue != null) {
      hashCode = 17 * hashCode + currentValue.hashCode();
    }
    if (goalValue != null) {
      hashCode = 17 * hashCode + goalValue.hashCode();
    }
    return hashCode;
  }

  /* (non-Javadoc)
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo(ProgressionValue o) {
    if (o == null) {
      return -1;
    }
    if (currentValue == null) {
      if (o.currentValue != null) {
        return -1;
      }
    } else if (!currentValue.equals(o.currentValue)) {
      if (o.currentValue == null) {
        return 1;
      }
      return currentValue.compareTo(o.currentValue);
    }
    if (goalValue == null) {
      if (o.goalValue != null) {
        return -1;
      }
    } else if (!goalValue.equals(o.goalValue)) {
      if (o.goalValue == null) {
        return 1;
      }
      return goalValue.compareTo(o.goalValue);
    }
    return 0;
  }
}
