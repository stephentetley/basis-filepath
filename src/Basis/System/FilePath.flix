/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


namespace Basis/System {

    ///
    /// Represents a FilePath.
    ///
    pub enum FilePath(Interop/NIO/File.Path)

    // Design note - can't be an alias as we want to define instances.


    /// Note formatting is system dependent.
    instance ToString[FilePath] {
        pub def toString(path: FilePath): String =
            let FilePath(path1) = path;
            Interop/NIO/File/Path.toString(path1)
    }

/*
    instance FromString[FilePath] {
        pub def fromString(s: String): Option[FilePath] =
            region r {
                try { 
                    import static java.nio.file.Path.of(String, Array[String, r]): ##java.nio.file.Path & r as of1;
                    Some(FilePath(of1(s, [] @ r)))
                } catch {
                    case _: ##java.lang.Exception => None
                }
            }
    }
*/

    instance Eq[FilePath] {
        pub def eq(x: FilePath, y: FilePath): Bool =
            let FilePath(x1) = x;
            let FilePath(y1) = y;
            Interop/NIO/File/Path.equals(x1, y1)
    }

/*
    instance SemiGroup[FilePath] {
        pub def combine(x: FilePath, y: FilePath): FilePath = Basis/System/FilePath.append(x, y)
    }
*/

    instance Order[FilePath] {
        pub def compare(x: FilePath, y: FilePath): Comparison =
            let FilePath(x1) = x;
            let FilePath(y1) = y;
            Interop/NIO/File/Path.compareTo(x1, y1) |> Comparison.fromInt32
    }

    instance Hash[FilePath] {
        pub def hash(path: FilePath): Int32 =
            let FilePath(path1) = path;
            Interop/NIO/File/Path.hashCode(path1)
    }

    instance Boxable[FilePath]

    instance Interop/Flix.Marshal[FilePath] {
        pub def toJavaObject(path: FilePath): ##java.lang.Object = 
            let FilePath(path1) = path;
            Interop/Flix/Marshal.toJavaObject(path1)
        
        pub def fromJavaObject(o: ##java.lang.Object): FilePath = 
            FilePath(Interop/Flix/Marshal.fromJavaObject(o))
    }

}

namespace Basis/System/FilePath {

    use ToString.toString;

    use Basis/System.FilePath;
    use Basis/System.FilePath.{FilePath};

/*
    /// Unwrap
    pub def getHH(p: FilePath): ##java.nio.file.Path = 
        let FilePath(p1) = p;
        p1
*/


    def getPath(p: FilePath): Interop/NIO/File.Path = 
        let FilePath(p1) = p;
        p1



    ///
    /// Returns a new FilePath.
    ///
    pub def filePath(s: String): Result[FilePath, String] =
        region r {
            Interop/NIO/File/Path.of(s, [] @ r) |> Result.map(FilePath)
        }

    def filePath1(s: String): FilePath = match filePath(s) {
        case Ok(p1)  => p1
        case Err(_)  => empty()
    }

/*
    /// pub def filePathConcat(l: List[String]): FilePath =
    ///     region r {
    ///         import static java.nio.file.Path.of(String, Array[String, r]): ##java.nio.file.Path & r as of1;
    ///         try {
    ///             match l { 
    ///                 case x :: rs => FilePath(FlixSandbox/Deprecated/Marshal.runVaragsOfString(rs, of1(x), r)) as & Pure
    ///                 case Nil     => empty()
    ///             }
    ///         } catch {
    ///             case _: ##java.lang.Exception => empty()
    ///         }
    ///     }


    pub def tryNew(s: String): Result[FilePath, String] =
        region r {
            import static java.nio.file.Path.of(String, Array[String, r]): ##java.nio.file.Path & r as of1;
            try {
                
                Ok(FilePath(of1(s, [] @ r)))
            } catch {
                case _: ##java.lang.Exception => Err("Invalid FilePath")
            }
        }
*/

    pub def empty(): FilePath =        
        match filePath("") {
            case Ok(p1) => p1
            case Err(msg) => bug!(msg)
        }


    pub def normalize(path: FilePath): FilePath =
        getPath(path) |> Interop/NIO/File/Path.normalize |> FilePath


    /// If `path` is a relative path `toAbsolutePath` resolves the path in a system specific manner.
    /// Should this be Impure?
    pub def toAbsolutePath(path: FilePath): Result[FilePath, String] =
        getPath(path) |> Interop/NIO/File/Path.toAbsolutePath |> Result.map(FilePath)


    pub def isAbsolute(path: FilePath): Bool =
        getPath(path) |> Interop/NIO/File/Path.isAbsolute

    pub def isRelative(path: FilePath): Bool = not isAbsolute(path)

    pub def getRoot(path: FilePath): Option[FilePath] =
        getPath(path) |> Interop/NIO/File/Path.getRoot |> Option.map(FilePath)

    pub def getParent(path: FilePath): Option[FilePath] =
        getPath(path) |> Interop/NIO/File/Path.getParent |> Option.map(FilePath)

    pub def getFileName(path: FilePath): Option[FilePath] =
        getPath(path) |> Interop/NIO/File/Path.getFileName |> Option.map(FilePath)

    pub def splitFileName(path: FilePath): (Option[FilePath], Option[FilePath]) =
        (getParent(path), getFileName(path))

    pub def replaceFileName(path: FilePath, name: String): FilePath =
        let file = filePath1(name);
        match getParent(path) {
            case Some(dir) => append(dir, file)
            case None => file
        }

    /// e.g. getBaseName('/home/stephen/temp01.txt') = "temp01"
    pub def getBaseName(path: FilePath): Option[String] =
        use Option.flatMap;
        let* file = getFileName(path);
        let base = dropExtensions(file);
        Some(toString(base))

    pub def putBaseName(path: FilePath, name: String): FilePath =
        match (getParent(path), getExtensions(path)) {
            case (Some(root), Some(exts)) => append(root, filePath1(name + "." + exts))
            case (None, Some(exts))       => filePath1(name + "." + exts)
            case (_, _)                   => path
        }

    pub def mapBaseName(f: String -> String, path: FilePath): FilePath =
        match getBaseName(path) {
            case Some(name1) => putBaseName(path, f(name1))
            case _ => path
        }

    pub def testBaseName(f: String -> Bool, path: FilePath): Bool =
        match getBaseName(path) {
            case Some(name1) => f(name1)
            case _ => false
        }


    pub def resolve(path: FilePath, other: FilePath): FilePath =
        let FilePath(path1) = path;
        let FilePath(other1) = other;
        Interop/NIO/File/Path.resolve(path1, other1) |> FilePath


    pub def append(path: FilePath, other: FilePath): FilePath =
        normalize(resolve(path, other))

    pub def concat(xs: List[FilePath]): FilePath = match xs {
        case Nil => empty()
        case root :: rs => normalize(List.foldLeft(resolve, root, rs))
    }

    pub def <<>>(path: FilePath, name1: String): FilePath = snoc(path, name1)

    pub def snoc(path: FilePath, name1: String): FilePath =
        match filePath(name1) {
            case Ok(path1) => append(path, path1)
            case Err(_) => path
        }

    pub def push(name1: String, path: FilePath): FilePath =
        match filePath(name1) {
            case Ok(path1) => append(path, path1)
            case Err(_) => path
        }


    ///
    /// Returns `path1` with the path name contracted relative to `path2`.
    ///
    pub def makeRelative(path1: FilePath, path2: FilePath): FilePath =
        let FilePath(p1) = path1;
        let FilePath(p2) = path2;
        Interop/NIO/File/Path.relativize(p1, p2) |> FilePath

    ///
    /// Returns `true` if the filepath `path1` starts with the string `path2`.
    ///
    pub def startsWith(prefix: {prefix :: FilePath}, path2: FilePath): Bool =
        let FilePath(prefix1) = prefix.prefix;
        let FilePath(p2) = path2;
        Interop/NIO/File/Path.startsWith(prefix = prefix1, p2)


    pub def getNameCount(path: FilePath): Int32 =
        match getRoot(path) {
            case Some(_) => getNameCountHelper(path) + 1
            case None => getNameCountHelper(path)
        }

    /// Does not count root, if present.
    def getNameCountHelper(path: FilePath): Int32 =
        getPath(path) |> Interop/NIO/File/Path.getNameCount

    pub def getNameAt(path: FilePath, ix: Int32): Option[FilePath] =
        if (ix < 0)
            None
        else
            match getRoot(path) {
                case Some(_) => if (ix == 0) getRoot(path) else getNameAtHelper(path, ix-1)
                case None => getNameAtHelper(path, ix)
            }
    ///
    /// Helper function for `getNameAt`.
    ///
    /// This is a wrapper around the Java function `getName` which does not
    /// look at root, if present.
    ///
    def getNameAtHelper(path: FilePath, ix: Int32): Option[FilePath] =
        getPath(path) |> Interop/NIO/File/Path.getNameAt(ix) |> Result.toOption |> Option.map(FilePath)

    pub def hasExtension(path: FilePath): Bool =
        match getFileName(path) |> Option.map(toString) {
            case None => false
            case Some(str) => String.startsWith({prefix = "."}, str)
        }


    pub def getExtension(path: FilePath): Option[String] =
        use Option.flatMap;
        let* s = getFileName(path) |> Option.map(toString);
        let* ix = String.indexOfRight({substr = "."}, s);
        Some(String.sliceRight(ix+1, s))

    ///
    /// Replaces the extension.
    /// If the filepath has multiple extenstion only the rightmost extension
    /// is replaced.
    ///
    pub def putExtension(path: FilePath, ext: String): FilePath =
        match getExtension(path) {
            case Some(ext0) => {
                let str = toString(path);
                let lhs = String.dropRight(String.length(ext0), str);
                filePath1(lhs + ext)
            }
            case None => path
        }

    ///
    /// Drop only the rightmost extension if the filepath has mutliple
    /// extensions, i.e. "archive.tar.gz" becomes "archive.tar".
    ///
    pub def dropExtension(path: FilePath): FilePath = match getExtension(path) {
        case Some(ext) => {
            let str = toString(path);
            let lhs = String.dropRight(String.length(ext) + 1, str);
            filePath1(lhs)
        }
        case None => path
    }

    pub def splitExtension(path: FilePath): (FilePath, Option[String]) =
        (dropExtension(path), getExtension(path))

    pub def mapExtension(f: String -> String, path: FilePath): FilePath =
        let (path1, ext) = splitExtension(path);
        match ext {
            case Some(s1) => addExtension(path1, f(s1))
            case None => path
        }

    /// false if no extension
    pub def testExtension(f: String -> Bool, path: FilePath): Bool =
        match getExtension(path) {
            case Some(s1) => f(s1)
            case None => false
        }

    pub def isExtension(path: FilePath, ext: String): Bool =
        testExtension(x -> x == ext, path)

    pub def isExtensionOneOf(path: FilePath, xs: List[String]): Bool =
        testExtension(x -> List.memberOf(x, xs), path)

    /// Suffixes the extension `ext` even if the path already has an extension.
    /// e.g. addExtension("archive.tar", "gz") == "archive.tar.gz"
    pub def addExtension(path: FilePath, ext: String): FilePath =
        if (String.isEmpty(ext))
            path
        else {
            let str = toString(path);
            filePath1(str + "." + ext)
        }

    ///
    /// Retuen all extensions if the filepath has multiple extensions,
    /// i.e. "archive.tar.gz" becomes "tar.gz".
    ///
    pub def getExtensions(path: FilePath): Option[String] =
        use Option.flatMap;
        let* s = getFileName(path) |> Option.map(toString);
        let* ix = String.indexOfLeft({substr = "."}, s);
        Some(String.sliceRight(ix + 1, s))

    ///
    /// Drop all extensions if the filepath has mutliple extensions,
    /// i.e. "archive.tar.gz" becomes "archive".
    ///
    pub def dropExtensions(path: FilePath): FilePath = match getExtensions(path) {
        case Some(ext) => {
            let str = toString(path);
            let lhs = String.dropRight(String.length(ext) + 1, str);
            filePath1(lhs)
        }
        case None => path
    }

    pub def putExtensions(path: FilePath, ext: String): FilePath =
        match getExtensions(path) {
            case Some(ext0) => {
                let str = toString(path);
                let lhs = String.dropRight(String.length(ext0), str);
                filePath1(lhs + ext)
            }
            case None => path
        }

    pub def splitExtensions(path: FilePath): (FilePath, Option[String]) =
        (dropExtensions(path), getExtensions(path))

    ///
    /// Drop the extension `ext` from the filepath and the dot preceeding it.
    /// If the extension of the filepath does not match ext or the filepath
    /// does not have an extension return None.
    ///
    pub def stripExtension(path: FilePath, ext: String): Option[FilePath] =
        use Option.flatMap;
        let* ext1 = getExtension(path);
        if (ext1 == ext)
            Some(dropExtension(path))
        else
            None

    /// Multiple extension version of `stripExtension`.
    pub def stripExtensions(path: FilePath, exts: String): Option[FilePath] =
        use Option.flatMap;
        let* exts1 = getExtensions(path);
        if (exts1 == exts)
            Some(dropExtensions(path))
        else
            None

    pub def split(path: FilePath): List[FilePath] =
        let n = getNameCount(path);
        splitHelper(path, n, 0, xs -> xs)

    def splitHelper(path: FilePath, count: Int32, i: Int32, k: List[FilePath] -> List[FilePath]): List[FilePath] =
        if (i >= count)
            k(Nil)
        else
            // Note - `None` case should be impossible.
            match getNameAt(path, i) {
                case Some(x) => splitHelper(path, count, i+1, xs -> k(x :: xs))
                case None => k(Nil)
            }
/*
    def pathMatcherMatches(path: FilePath, syntaxAndPatt: String): Result[Bool, String] & Impure =
        import static java.nio.file.FileSystems.getDefault(): ##java.nio.file.FileSystem & Impure;
        import java.nio.file.FileSystem.getPathMatcher(String): ##java.nio.file.PathMatcher & Impure;
        import java.nio.file.PathMatcher.matches(##java.nio.file.Path): Bool & Impure;
        try {
            let FilePath(path1) = path;
            let fs = getDefault();
            let m1 = getPathMatcher(fs, syntaxAndPatt);
            Ok(matches(m1, path1))
        } catch {
            case _: ##java.lang.Exception => Err("pathMatcherMatches")
        }

    pub def isGlobMatch(glob: String, path: FilePath): Result[Bool, String] =
        pathMatcherMatches(path, "glob:" + glob) as & Pure

    pub def isRegexMatch(patt: String, path: FilePath): Result[Bool, String] =
        pathMatcherMatches(path, "regex:" + patt) as & Pure

*/
    pub def toFile(r: Region[r], path: FilePath): Result[Interop/IO.File[r], String] \ Write(r) =
        getPath(path) |> Interop/NIO/File/Path.toFile(r)

    pub def fromFile(file: Interop/IO.File[r]): Result[FilePath, String] \ Read(r) =
        Interop/IO/File.toPath(file) |> Result.map(FilePath)

}
