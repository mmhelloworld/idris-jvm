package io.github.mmhelloworld.idrisjvm;

import idris.prelude.list.ListExport;
import idris.prelude.list.ListFDesc;
import idris.prelude.list.ListLVar;
import idris.prelude.list.ListSAlt;
import idris.prelude.list.ListString;
import idrisjvm.ir.Export;
import idrisjvm.ir.FDesc;
import idrisjvm.core.JCodegen;
import idrisjvm.ir.LVar;
import idrisjvm.ir.MaybeLVar;
import idrisjvm.ir.SAlt;
import idrisjvm.ir.SForeignArg;
import idrisjvm.ir.SForeignArgs;

import java.util.List;
import java.util.ListIterator;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

public class Converters {

    public static ListFDesc toIdrisListFDesc(List<FDesc> jlist) {
        return toIdrisList(jlist, JCodegen::emptyFDesc, JCodegen::consFDesc);
    }

    public static ListExport toIdrisListExport(List<Export> jlist) {
        return toIdrisList(jlist, JCodegen::emptyExport, JCodegen::consExport);
    }

    public static ListString toIdrisListString(List<String> jlist) {
        return toIdrisList(jlist, JCodegen::emptyListString, JCodegen::consString);
    }

    public static ListLVar toIdrisListLVar(List<LVar> jlist) {
        return toIdrisList(jlist, JCodegen::emptyLVar, JCodegen::consLVar);
    }

    public static ListSAlt toIdrisListSAlt(List<SAlt> jlist) {
        return toIdrisList(jlist, JCodegen::emptySAlt, JCodegen::consSAlt);
    }

    public static SForeignArgs toIdrisListSForeignArgs(List<SForeignArg> jlist) {
        return toIdrisList(jlist, JCodegen::emptySForeignArg, JCodegen::consSForeignArg);
    }

    public static MaybeLVar nullableToMaybeLVar(LVar nullableLVar) {
        return nullableToMaybe(nullableLVar, JCodegen::justLVar, JCodegen::nothingLVar);
    }

    private static <JType, IdrisType> IdrisType nullableToMaybe(JType nullableValue,
                                                               Function<JType, IdrisType> justSupplier,
                                                               Supplier<IdrisType> nothingSupplier) {
        return Optional.ofNullable(nullableValue)
            .map(justSupplier)
            .orElseGet(nothingSupplier);
    }

    private static <E, T> T toIdrisList(List<E> xs, Supplier<T> emptySupplier, BiFunction<E, T, T> cons) {
        final ListIterator<E> reverseItr = xs.listIterator(xs.size());
        T res = emptySupplier.get();
        while (reverseItr.hasPrevious()) {
            res = cons.apply(reverseItr.previous(), res);
        }
        return res;
    }
}
