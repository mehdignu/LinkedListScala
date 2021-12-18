package com.rockthejvm

package object test {

  class Writer(firstName: String, surName: String, val year: Int) {
    def fullName: String = firstName + " " + surName
  }

  class Novel(name: String, yearOfRelease: Int, author: Writer) {
    def authorAge: Int = this.author.year
    def isWrittenBy(author: Writer): String = author.fullName
    def copy(newYear: Int): Novel = new Novel(name, newYear, author)
  }


}
