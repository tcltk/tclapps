namespace eval base64 {
    proc encode {args} {binary encode base64 {*}$args}
    proc decode {data} {binary decode base64 $data}
}
package provide base64 99.0